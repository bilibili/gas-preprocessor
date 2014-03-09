#!/usr/bin/env perl
# by David Conrad
# This code is licensed under GPLv2 or later; go to gnu.org to read it
#  (not that it much matters for an asm preprocessor)
# usage: set your assembler to be something like "perl gas-preprocessor.pl gcc"
use strict;

# Apple's gas is ancient and doesn't support modern preprocessing features like
# .rept and has ugly macro syntax, among other things. Thus, this script
# implements the subset of the gas preprocessor used by x264 and ffmpeg
# that isn't supported by Apple's gas.

my %canonical_arch = ("aarch64" => "aarch64", "arm64" => "aarch64",
                      "arm"     => "arm",
                      "powerpc" => "powerpc", "ppc"   => "powerpc");

my %comments = ("aarch64" => '//',
                "arm"     => '@',
                "powerpc" => '#');

my @options;
my @gcc_cmd;
my @preprocess_c_cmd;

my $comm;
my $arch;
my $as_type = "apple-gas";

my $fix_unreq = $^O eq "darwin";

my $usage_str = "
$0\n
Gas-preprocessor.pl converts assembler files using modern GNU as syntax for
Apple's ancient gas version or clang's incompatible integrated assembler. The
conversion is regularly tested for Libav, x264 and vlc. Other projects might
use different features which are not correctly handled.

Options for this program needs to be separated with ' -- ' from the assembler
command. Following options are currently supported:

    -help         - this usage text
    -arch         - target architecture
    -as-type      - one value out of {,apple-}{gas,clang}
    -fix-unreq
    -no-fix-unreq
";

sub usage() {
    print $usage_str;
}

while (@ARGV) {
    my $opt = shift;
    last if ($opt =~ /^--$/);
    push @options, $opt;
}
if (@ARGV) {
    @gcc_cmd = @ARGV;
} else {
    @gcc_cmd = @options;
    @options = ();

    # backward compatible handling
    if ($gcc_cmd[0] eq "-fix-unreq") {
        $fix_unreq = 1;
        shift @gcc_cmd;
    } elsif ($gcc_cmd[0] eq "-no-fix-unreq") {
        $fix_unreq = 0;
        shift @gcc_cmd;
    }
}

while (@options) {
    my $opt = shift @options;
    if ($opt =~ /^-(no-)?fix-unreq$/) {
        $fix_unreq = $1 ne "no-";
    } elsif ($opt eq "-arch") {
        $arch = shift @options;
        die "unkown arch: '$arch'\n" if not exists $comments{$arch};
    } elsif ($opt eq "-as-type") {
        $as_type = shift @options;
        die "unkown as type: '$as_type'\n" if $as_type !~ /^(apple-)?(gas|clang)$/;
    } elsif ($opt eq "-help") {
        usage();
        exit 0;
    } else {
        usage();
        die "option '$opt' is not known\n";
    }
}

if (grep /\.c$/, @gcc_cmd) {
    # C file (inline asm?) - compile
    @preprocess_c_cmd = (@gcc_cmd, "-S");
} elsif (grep /\.[sS]$/, @gcc_cmd) {
    # asm file, just do C preprocessor
    @preprocess_c_cmd = (@gcc_cmd, "-E");
} elsif (grep /-(v|-version)/, @gcc_cmd) {
    # pass -v/--version along, used during probing. Matching '-v' might have
    # uninteded results but it doesn't matter much if gas-preprocessor or
    # the compiler fails.
    exec(@gcc_cmd);
} else {
    die "Unrecognized input filetype";
}

# if compiling, avoid creating an output file named '-.o'
if ((grep /^-c$/, @gcc_cmd) && !(grep /^-o/, @gcc_cmd)) {
    foreach my $i (@gcc_cmd) {
        if ($i =~ /\.[csS]$/) {
            my $outputfile = $i;
            $outputfile =~ s/\.[csS]$/.o/;
            push(@gcc_cmd, "-o");
            push(@gcc_cmd, $outputfile);
            last;
        }
    }
}
@gcc_cmd = map { /\.[csS]$/ ? qw(-x assembler -) : $_ } @gcc_cmd;
@preprocess_c_cmd = map { /\.o$/ ? "-" : $_ } @preprocess_c_cmd;

# detect architecture from gcc binary name
if (!$arch) {
    if ($gcc_cmd[0] =~ /(arm64|aarch64|arm|powerpc|ppc)/) {
        $arch = $1;
    } else {
        # look for -arch flag
        foreach my $i (1 .. $#gcc_cmd-1) {
            if ($gcc_cmd[$i] eq "-arch" and
                $gcc_cmd[$i+1] =~ /(arm64|aarch64|arm|powerpc|ppc)/) {
                $arch = $1;
            }
        }
    }
}

# assume we're not cross-compiling if no -arch or the binary doesn't have the arch name
$arch = qx/arch/ if (!$arch);

die "Unknown target architecture '$arch'" if not exists $canonical_arch{$arch};

$arch = $canonical_arch{$arch};
$comm = $comments{$arch};

my %ppc_spr = (ctr    => 9,
               vrsave => 256);

open(ASMFILE, "-|", @preprocess_c_cmd) || die "Error running preprocessor";

my $current_macro = '';
my $macro_level = 0;
my %macro_lines;
my %macro_args;
my %macro_args_default;
my $macro_count = 0;
my $altmacro = 0;
my $in_irp = 0;

my @pass1_lines;
my @ifstack;

my %symbols;

# pass 1: parse .macro
# note that the handling of arguments is probably overly permissive vs. gas
# but it should be the same for valid cases
while (<ASMFILE>) {
    # remove all comments (to avoid interfering with evaluating directives)
    s/(?<!\\)$comm.*//x;

    # comment out unsupported directives
    s/\.type/$comm$&/x;
    s/\.func/$comm$&/x;
    s/\.endfunc/$comm$&/x;
    s/\.ltorg/$comm$&/x;
    s/\.size/$comm$&/x;
    s/\.fpu/$comm$&/x;
    s/\.arch/$comm$&/x;
    s/\.object_arch/$comm$&/x;

    # the syntax for these is a little different
    s/\.global/.globl/x;
    # also catch .section .rodata since the equivalent to .const_data is .section __DATA,__const
    s/(.*)\.rodata/.const_data/x;
    s/\.int/.long/x;
    s/\.float/.single/x;

    # catch unknown section names that aren't mach-o style (with a comma)
    if (/.section ([^,]*)$/) {
        die ".section $1 unsupported; figure out the mach-o section name and add it";
    }

    parse_line($_);
}

sub eval_expr {
    my $expr = $_[0];
    while ($expr =~ /([A-Za-z._][A-Za-z0-9._]*)/g) {
        my $sym = $1;
        $expr =~ s/$sym/$symbols{$sym}/ if defined $symbols{$sym};
    }
    eval $expr;
}

sub handle_if {
    my $line = $_[0];
    # handle .if directives; apple's assembler doesn't support important non-basic ones
    # evaluating them is also needed to handle recursive macros
    if ($line =~ /\.if(n?)([a-z]*)\s+(.*)/) {
        my $result = $1 eq "n";
        my $type   = $2;
        my $expr   = $3;

        if ($type eq "b") {
            $expr =~ s/\s//g;
            $result ^= $expr eq "";
        } elsif ($type eq "c") {
            if ($expr =~ /(.*)\s*,\s*(.*)/) {
                $result ^= $1 eq $2;
            } else {
                die "argument to .ifc not recognized";
            }
        } elsif ($type eq "") {
            $result ^= eval_expr($expr) != 0;
        } elsif ($type eq "eq") {
            $result = eval_expr($expr) == 0;
        } elsif ($type eq "lt") {
            $result = eval_expr($expr) < 0;
        } else {
            chomp($line);
            die "unhandled .if varient. \"$line\"";
        }
        push (@ifstack, $result);
        return 1;
    } else {
        return 0;
    }
}

sub parse_if_line {
    my $line = @_[0];

    # evaluate .if blocks
    if (scalar(@ifstack)) {
        if ($line =~ /\.endif/) {
            pop(@ifstack);
            return 1;
        } elsif ($line =~ /\.elseif\s+(.*)/) {
            if ($ifstack[-1] == 0) {
                $ifstack[-1] = !!eval_expr($1);
            } elsif ($ifstack[-1] > 0) {
                $ifstack[-1] = -$ifstack[-1];
            }
            return 1;
        } elsif ($line =~ /\.else/) {
            $ifstack[-1] = !$ifstack[-1];
            return 1;
        } elsif (handle_if($line)) {
            return 1;
        }

        # discard lines in false .if blocks
        foreach my $i (0 .. $#ifstack) {
            if ($ifstack[$i] <= 0) {
                return 1;
            }
        }
    }
    return 0;
}

sub parse_line {
    my $line = @_[0];

    return if (parse_if_line($line));

    if (/\.macro/) {
        $macro_level++;
        if ($macro_level > 1 && !$current_macro) {
            die "nested macros but we don't have master macro";
        }
    } elsif (/\.endm/) {
        $macro_level--;
        if ($macro_level < 0) {
            die "unmatched .endm";
        } elsif ($macro_level == 0) {
            $current_macro = '';
            return;
        }
    } elsif (/\.irp/ or /\.rept/) {
        $in_irp = 1;
    } elsif (/.endr/) {
        $in_irp = 0;
    }

    if ($macro_level > 1) {
        push(@{$macro_lines{$current_macro}}, $line);
    } elsif ($macro_level == 0) {
        expand_macros($line);
    } else {
        if ($line =~ /\.macro\s+([\d\w\.]+)\s*(.*)/) {
            $current_macro = $1;

            # commas in the argument list are optional, so only use whitespace as the separator
            my $arglist = $2;
            $arglist =~ s/,/ /g;

            my @args = split(/\s+/, $arglist);
            foreach my $i (0 .. $#args) {
                my @argpair = split(/=/, $args[$i]);
                $macro_args{$current_macro}[$i] = $argpair[0];
                $argpair[0] =~ s/:vararg$//;
                $macro_args_default{$current_macro}{$argpair[0]} = $argpair[1];
            }
            # ensure %macro_lines has the macro name added as a key
            $macro_lines{$current_macro} = [];

        } elsif ($current_macro) {
            push(@{$macro_lines{$current_macro}}, $line);
        } else {
            die "macro level without a macro name";
        }
    }
}

sub handle_set {
    my $line = $_[0];
    if ($line =~ /\.set\s+(.*),\s*(.*)/) {
        $symbols{$1} = eval_expr($2);
    }
}

sub expand_macros {
    my $line = @_[0];

    # handle .if directives; apple's assembler doesn't support important non-basic ones
    # evaluating them is also needed to handle recursive macros
    if (!$in_irp && handle_if($line)) {
        return;
    }

    if (/\.purgem\s+([\d\w\.]+)/) {
        delete $macro_lines{$1};
        delete $macro_args{$1};
        delete $macro_args_default{$1};
        return;
    }

    if ($line =~ /\.altmacro/) {
        $altmacro = 1;
        return;
    }

    if ($line =~ /\.noaltmacro/) {
        $altmacro = 0;
        return;
    }

    $line =~ s/\%([^,]*)/eval_expr($1)/eg if $altmacro;

    handle_set($line);

    if ($line =~ /(\S+:|)\s*([\w\d\.]+)\s*(.*)/ && exists $macro_lines{$2}) {
        push(@pass1_lines, $1);
        my $macro = $2;

        # commas are optional here too, but are syntactically important because
        # parameters can be blank
        my @arglist = split(/,/, $3);
        my @args;
        my @args_seperator;

        my $comma_sep_required = 0;
        foreach (@arglist) {
            # allow arithmetic/shift operators in macro arguments
            $_ =~ s/\s*(\+|-|\*|\/|<<|>>)\s*/$1/g;

            my @whitespace_split = split(/\s+/, $_);
            if (!@whitespace_split) {
                push(@args, '');
                push(@args_seperator, '');
            } else {
                foreach (@whitespace_split) {
                        #print ("arglist = \"$_\"\n");
                    if (length($_)) {
                        push(@args, $_);
                        my $sep = $comma_sep_required ? "," : " ";
                        push(@args_seperator, $sep);
                        #print ("sep = \"$sep\", arg = \"$_\"\n");
                        $comma_sep_required = 0;
                    }
                }
            }

            $comma_sep_required = 1;
        }

        my %replacements;
        if ($macro_args_default{$macro}){
            %replacements = %{$macro_args_default{$macro}};
        }

        # construct hashtable of text to replace
        foreach my $i (0 .. $#args) {
            my $argname = $macro_args{$macro}[$i];
            my @macro_args = @{ $macro_args{$macro} };
            if ($args[$i] =~ m/=/) {
                # arg=val references the argument name
                # XXX: I'm not sure what the expected behaviour if a lot of
                # these are mixed with unnamed args
                my @named_arg = split(/=/, $args[$i]);
                $replacements{$named_arg[0]} = $named_arg[1];
            } elsif ($i > $#{$macro_args{$macro}}) {
                # more args given than the macro has named args
                # XXX: is vararg allowed on arguments before the last?
                $argname = $macro_args{$macro}[-1];
                if ($argname =~ s/:vararg$//) {
                    #print "macro = $macro, args[$i] = $args[$i], args_seperator=@args_seperator, argname = $argname, arglist[$i] = $arglist[$i], arglist = @arglist, args=@args, macro_args=@macro_args\n";
                    #$replacements{$argname} .= ", $args[$i]";
                    $replacements{$argname} .= "$args_seperator[$i] $args[$i]";
                } else {
                    die "Too many arguments to macro $macro";
                }
            } else {
                $argname =~ s/:vararg$//;
                $replacements{$argname} = $args[$i];
            }
        }

        my $count = $macro_count++;

        # apply replacements as regex
        foreach (@{$macro_lines{$macro}}) {
            my $macro_line = $_;
            # do replacements by longest first, this avoids wrong replacement
            # when argument names are subsets of each other
            foreach (reverse sort {length $a <=> length $b} keys %replacements) {
                $macro_line =~ s/\\$_/$replacements{$_}/g;
            }
            $macro_line =~ s/\\\@/$count/g;
            $macro_line =~ s/\\\(\)//g;     # remove \()
            parse_line($macro_line);
        }
    } else {
        push(@pass1_lines, $line);
    }
}

close(ASMFILE) or exit 1;
if ($ENV{GASPP_DEBUG}) {
    open(ASMFILE, ">&STDOUT");
} else {
    open(ASMFILE, "|-", @gcc_cmd) or die "Error running assembler";
}

my @sections;
my $num_repts;
my @rept_lines;

my %literal_labels;     # for ldr <reg>, =<expr>
my $literal_num = 0;
my $literal_expr = ".word";
$literal_expr = ".quad" if $arch eq "aarch64";

my $thumb = 0;

my %thumb_labels;
my %call_targets;

my @irp_args;
my $irp_param;

my %neon_alias_reg;
my %neon_alias_type;

my %aarch64_req_alias;

# pass 2: parse .rept and .if variants
foreach my $line (@pass1_lines) {
    # handle .previous (only with regard to .section not .subsection)
    if ($line =~ /\.(section|text|const_data)/) {
        push(@sections, $line);
    } elsif ($line =~ /\.previous/) {
        if (!$sections[-2]) {
            die ".previous without a previous section";
        }
        $line = $sections[-2];
        push(@sections, $line);
    }

    $thumb = 1 if $line =~ /\.code\s+16|\.thumb/;
    $thumb = 0 if $line =~ /\.code\s+32|\.arm/;

    # handle ldr <reg>, =<expr>
    if ($line =~ /(.*)\s*ldr([\w\s\d]+)\s*,\s*=(.*)/) {
        my $label = $literal_labels{$3};
        if (!$label) {
            $label = "Literal_$literal_num";
            $literal_num++;
            $literal_labels{$3} = $label;
        }
        $line = "$1 ldr$2, $label\n";
    } elsif ($line =~ /\.ltorg/) {
        $line .= ".align 2\n";
        foreach my $literal (keys %literal_labels) {
            $line .= "$literal_labels{$literal}:\n $literal_expr $literal\n";
        }
        %literal_labels = ();
    }

    # handle GNU as pc-relative relocations for adrp/add
    if ($line =~ /(.*)\s*adrp([\w\s\d]+)\s*,\s*#:pg_hi21:([^\s]+)/) {
        $line = "$1 adrp$2, ${3}\@PAGE\n";
    } elsif ($line =~ /(.*)\s*add([\w\s\d]+)\s*,([\w\s\d]+)\s*,\s*#:lo12:([^\s]+)/) {
        $line = "$1 add$2, $3, ${4}\@PAGEOFF\n";
    }

    # thumb add with large immediate needs explicit add.w
    if ($thumb and $line =~ /add\s+.*#([^@]+)/) {
        $line =~ s/add/add.w/ if eval_expr($1) > 255;
    }

    # mach-o local symbol names start with L (no dot)
    $line =~ s/(?<!\w)\.(L\w+)/$1/g;

    # recycle the commented '.func' directive for '.thumb_func'
    if ($thumb) {
        $line =~ s/$comm\.func/.thumb_func/x;
    }

    if ($thumb and $line =~ /^\s*(\w+)\s*:/) {
        $thumb_labels{$1}++;
    }

    if ($line =~ /^\s*((\w+\s*:\s*)?bl?x?(..)?(?:\.w)?|\.globl)\s+(\w+)/) {
        my $cond = $3;
        my $label = $4;
        # Don't interpret e.g. bic as b<cc> with ic as conditional code
        if ($cond =~ /|eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|le|al|hs|lo/) {
            if (exists $thumb_labels{$label}) {
                print ASMFILE ".thumb_func $label\n";
            } else {
                $call_targets{$label}++;
            }
        }
    }

    # @l -> lo16()  @ha -> ha16()
    $line =~ s/,\s+([^,]+)\@l\b/, lo16($1)/g;
    $line =~ s/,\s+([^,]+)\@ha\b/, ha16($1)/g;

    # move to/from SPR
    if ($line =~ /(\s+)(m[ft])([a-z]+)\s+(\w+)/ and exists $ppc_spr{$3}) {
        if ($2 eq 'mt') {
            $line = "$1${2}spr $ppc_spr{$3}, $4\n";
        } else {
            $line = "$1${2}spr $4, $ppc_spr{$3}\n";
        }
    }

    if ($line =~ /\.rept\s+(.*)/) {
        $num_repts = $1;
        @rept_lines = ("\n");

        # handle the possibility of repeating another directive on the same line
        # .endr on the same line is not valid, I don't know if a non-directive is
        if ($num_repts =~ s/(\.\w+.*)//) {
            push(@rept_lines, "$1\n");
        }
        $num_repts = eval_expr($num_repts);
    } elsif ($line =~ /\.irp\s+([\d\w\.]+)\s*(.*)/) {
        $in_irp = 1;
        $num_repts = 1;
        @rept_lines = ("\n");
        $irp_param = $1;

        # only use whitespace as the separator
        my $irp_arglist = $2;
        $irp_arglist =~ s/,/ /g;
        $irp_arglist =~ s/^\s+//;
        @irp_args = split(/\s+/, $irp_arglist);
    } elsif ($line =~ /\.irpc\s+([\d\w\.]+)\s*(.*)/) {
        $in_irp = 1;
        $num_repts = 1;
        @rept_lines = ("\n");
        $irp_param = $1;

        my $irp_arglist = $2;
        $irp_arglist =~ s/,/ /g;
        $irp_arglist =~ s/^\s+//;
        @irp_args = split(//, $irp_arglist);
    } elsif ($line =~ /\.endr/) {
        if ($in_irp != 0) {
            foreach my $i (@irp_args) {
                foreach my $origline (@rept_lines) {
                    my $line = $origline;
                    $line =~ s/\\$irp_param/$i/g;
                    $line =~ s/\\\(\)//g;     # remove \()
                    handle_serialized_line($line, 1);
                }
            }
        } else {
            for (1 .. $num_repts) {
                foreach my $origline (@rept_lines) {
                    my $line = $origline;
                    handle_serialized_line($line, 1);
                }
            }
        }
        @rept_lines = ();
        $in_irp = 0;
        @irp_args = '';
    } elsif (scalar(@rept_lines)) {
        push(@rept_lines, $line);
    } else {
        handle_serialized_line($line, 0);
    }
}

sub handle_serialized_line {
    my $line = @_[0];
    my $do_eval_if = @_[1];

    if ($do_eval_if) {
        return if parse_if_line($line);
        return if handle_if($line);
    }

    handle_set($line);

    if ($line =~ /\.unreq\s+(.*)/) {
        if (defined $neon_alias_reg{$1}) {
            delete $neon_alias_reg{$1};
            delete $neon_alias_type{$1};
            return;
        } elsif (defined $aarch64_req_alias{$1}) {
            delete $aarch64_req_alias{$1};
            return;
        }
    }
    # old gas versions store upper and lower case names on .req,
    # but they remove only one on .unreq
    if ($fix_unreq) {
        if ($line =~ /\.unreq\s+(.*)/) {
            $line = ".unreq " . lc($1) . "\n";
            $line .= ".unreq " . uc($1) . "\n";
        }
    }

    if ($line =~ /(\w+)\s+\.(dn|qn)\s+(\w+)(?:\.(\w+))?(\[\d+\])?/) {
        $neon_alias_reg{$1} = "$3$5";
        $neon_alias_type{$1} = $4;
        return;
    }
    if (scalar keys %neon_alias_reg > 0 && $line =~ /^\s+v\w+/) {
        # This line seems to possibly have a neon instruction
        foreach (keys %neon_alias_reg) {
            my $alias = $_;
            # Require the register alias to match as an invididual word, not as a substring
            # of a larger word-token.
            if ($line =~ /\b$alias\b/) {
                $line =~ s/\b$alias\b/$neon_alias_reg{$alias}/g;
                # Add the type suffix. If multiple aliases match on the same line,
                # only do this replacement the first time (a vfoo.bar string won't match v\w+).
                $line =~ s/^(\s+)(v\w+)(\s+)/$1$2.$neon_alias_type{$alias}$3/;
            }
        }
    }

    if ($arch eq "aarch64") {
        # clang's integrated aarch64 assembler in Xcode 5 does not support .req/.unreq
        if ($line =~ /\b(\w+)\s+\.req\s+(\w+)\b/) {
            $aarch64_req_alias{$1} = $2;
            return;
        }
        foreach (keys %aarch64_req_alias) {
            my $alias = $_;
            # recursively resolve aliases
            my $resolved = $aarch64_req_alias{$alias};
            while (defined $aarch64_req_alias{$resolved}) {
                $resolved = $aarch64_req_alias{$resolved};
            }
            $line =~ s/\b$alias\b/$resolved/g;
        }
        # fix missing aarch64 instructions in Xcode 5.1 (beta3)
        # mov with vector arguments is not supported, use alias orr instead
        if ($line =~ /^\s*mov\s+(v\d[\.{}\[\]\w]+),\s*(v\d[\.{}\[\]\w]+)\b\s*$/) {
            $line = "        orr $1, $2, $2\n";
        }
        # movi 8, 16, 32 bit shifted variant, shift is optional
        if ($line =~ /^\s*movi\s+(v[0-3]?\d\.(?:2|4|8|16)[bhsBHS])\s*,\s*(#\w+)\b\s*$/) {
            $line = "        movi $1, $2, lsl #0\n";
        }
        # Xcode 5 misses the alias uxtl replace it with the more general ushll
        if ($line =~ /^\s*uxtl(2)?\s+(v[0-3]?\d\.[248][hsdHSD])\s*,\s*(v[0-3]?\d\.(?:4|8|16)[bhsBHS])\b\s*$/) {
            $line = "        ushll$1 $2, $3, #0\n";
        }
        if ($ENV{GASPP_FIX_XCODE5}) {
            if ($line =~ /^\s*bsl\b/) {
                $line =~ s/\b(bsl)(\s+v[0-3]?\d\.(\w+))\b/$1.$3$2/;
                $line =~ s/\b(v[0-3]?\d)\.$3\b/$1/g;
            }
            if ($line =~ /^\s*saddl2?\b/) {
                $line =~ s/\b(saddl2?)(\s+v[0-3]?\d\.(\w+))\b/$1.$3$2/;
                $line =~ s/\b(v[0-3]?\d)\.\w+\b/$1/g;
            }
        }
    }

    print ASMFILE $line;
}

print ASMFILE ".text\n";
print ASMFILE ".align 2\n";
foreach my $literal (keys %literal_labels) {
    print ASMFILE "$literal_labels{$literal}:\n $literal_expr $literal\n";
}

map print(ASMFILE ".thumb_func $_\n"),
    grep exists $thumb_labels{$_}, keys %call_targets;

close(ASMFILE) or exit 1;
#exit 1
