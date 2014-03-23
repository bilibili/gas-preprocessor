.macro m param
    .if \param
        .macro inner
            mov r0, #1
        .endm
    .else
        .macro inner
            mov r0, #0
        .endm
    .endif

    inner
    mov r1, #\param

    .purgem inner
.endm

m 1
m 0

.irpc i, 0123
    mov r1, #\i
    .iflt \i-2
        mov r2, #42
    .else
        mov r3, #42
    .endif
.endr

.irpc i, 01
    m \i
.endr
