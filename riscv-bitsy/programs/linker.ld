OUTPUT_FORMAT("elf32-littleriscv", "elf32-littleriscv", "elf32-littleriscv")
OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS {
    . = 0x0;  /* Set the program entry point to address 0 */
    .text : {
        *(.text)    /* .text section contains program instructions */
    }
    .data : {
        *(.data)    /* .data section contains initialized data */
    }
    .bss : {
        *(.bss)     /* .bss section contains uninitialized data */
    }
}
