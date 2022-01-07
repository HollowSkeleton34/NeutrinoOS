section .boot

global __default_drive
__default_drive: db 0

global __num_sectors
__num_sectors: dd 86

bits 16
global boot
boot:
	mov ax, 0x2401
	int 0x15

.load_kernel:
	mov [__default_drive],dl

	mov ah, 0x2    ;read sectors
	mov al, [__num_sectors]      ;sectors to read
	mov ch, 0      ;cylinder idx
	mov dh, 0      ;head idx
	mov cl, 2      ;sector idx
	mov dl, [__default_drive] ;disk idx
	mov bx, target ;target pointer
	int 0x13

.setup_vesa:
	mov ax, 0x4F00
	mov di, __vbeInfoBlock
	int 0x10

	mov ax, 0x4F01
	mov cx, 0x011B
	mov di, __vbeModeBlock
	int 0x10

	mov ax, 0x4F02
	mov bx, 0x011B
	mov di, __vbeCrtcBlock
	int 0x10

.read_drives:
	mov eax, 0
	mov [__drive_count], eax

	mov eax, __drive_data
	mov [__temppointer], eax
	mov dl, 0x80
	mov ah, 0x08
	mov di, 0
.drive_loop:
	mov ah, 0x08
	int 0x13

	jc .drives_found

	; increment drive count
	mov eax, [__drive_count]
	inc eax
	mov [__drive_count], eax

	mov ebx, [__temppointer]

	movzx eax, dh
	add eax, 1
	mov [ebx], eax

	movzx eax, cx
	and eax, 0x3F
	mov [ebx+8], eax

	movzx eax, cx
	shr eax, 6
	and eax, 0x3FF
	add eax, 1
	mov [ebx+4], eax

	inc dl
	add ebx, 3

	mov [__temppointer], ebx
	jmp .drive_loop
.drives_found:
	mov [__temppointer], dword 0

.get_e820:
	mov di, word __e820_data

.e820_loop:
	mov eax, 0xe820
	mov ebx, [__tempcont]
	mov ecx, 20
	mov edx, 0x534D4150
	int 0x15

	mov [__tempcont], ebx

	jc .e820_found
	or ebx, ebx
	jz .e820_found

	mov eax, [__e820_size]
	inc eax
	mov [__e820_size], eax

	mov ax, di
	add ax, 20
	mov di, ax

	jmp .e820_loop

.e820_found:
    ; Checks if the cpuid instruction is available for use.
    ; If not, then the CPU will switch and stay in
    ; 32 bit Proteced Mode
    call CheckCPUID
    mov eax, 0x80000000                 ; Set the A-register to 0x80000000.
    cpuid                               ; CPU identification.
    cmp eax, 0x80000001                 ; Compare the A-register with 0x80000001.
    jb 32BitProtectedMode.NoLongMode    ; Switch CPU to 32 bit proteced mode.

    mov eax, 0x80000001                      ; Set the A-register to 0x80000001.
    cpuid                                    ; CPU identification.
    test edx, 1 << 29                        ; Test if the LM-bit, which is bit 29, is set in the D-register.
    jz 32BitProtectedMode.NoLongMode         ; Switch CPU to 32 bit proteced mode

    ; Disabling old paging first
    mov eax, cr0                                   ; Set the A-register to control register 0.
    and eax, 01111111111111111111111111111111b     ; Clear the PG-bit, which is bit 31.
    mov cr0, eax                                   ; Set control register 0 to the A-register.

    ; Clear paging tables
    mov edi, 0x1000    ; Set the destination index to 0x1000.
    mov cr3, edi       ; Set control register 3 to the destination index.
    xor eax, eax       ; Nullify the A-register.
    mov ecx, 4096      ; Set the C-register to 4096.
    rep stosd          ; Clear the memory.
    mov edi, cr3       ; Set the destination index to control register 3.

    ; Changing the location the first entry of the Page Map Level 4 Table is pointing to
    ; in the Page Directory Pointer Table
    mov DWORD [edi], 0x2003      ; Set the uint32_t at the destination index to 0x2003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.
    mov DWORD [edi], 0x3003      ; Set the uint32_t at the destination index to 0x3003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.
    mov DWORD [edi], 0x4003      ; Set the uint32_t at the destination index to 0x4003.
    add edi, 0x1000              ; Add 0x1000 to the destination index.

    mov ebx, 0x00000003          ; Set the B-register to 0x00000003.
    mov ecx, 512                 ; Set the C-register to 512.

.SetEntry:
    mov DWORD [edi], ebx         ; Set the uint32_t at the destination index to the B-register.
    add ebx, 0x1000              ; Add 0x1000 to the B-register.
    add edi, 8                   ; Add eight to the destination index.
    loop .SetEntry               ; Set the next entry.
    mov eax, cr4                 ; Set the A-register to control register 4.
    or eax, 1 << 5               ; Set the PAE-bit, which is the 6th bit (bit 5).
    mov cr4, eax                 ; Set control register 4 to the A-register.

.RealToProtected:
    mov ecx, 0xC0000080          ; Set the C-register to 0xC0000080, which is the EFER MSR.
    rdmsr                        ; Read from the model-specific register.
    or eax, 1 << 8               ; Set the LM-bit which is the 9th bit (bit 8).
    wrmsr                        ; Write to the model-specific register.
    mov eax, cr0                 ; Set the A-register to control register 0.
    or eax, 1 << 31 | 1 << 0     ; Set the PG-bit, which is the 31nd bit, and the PM-bit, which is the 0th bit.
    mov cr0, eax                 ; Set control register 0 to the A-register.

global 32BitProtectedMode
32BitProtectedMode:
    mov ecx, 0xC0000080          ; Set the C-register to 0xC0000080, which is the EFER MSR.
    rdmsr                        ; Read from the model-specific register.
    or eax, 1 << 8               ; Set the LM-bit which is the 9th bit (bit 8).
    wrmsr
    mov eax, cr0                 ; Set the A-register to control register 0.
    or eax, 1 << 31              ; Set the PG-bit, which is the 32nd bit (bit 31).
    mov cr0, eax                 ; Set control register 0 to the A-register.
    jmp .LongMode

.NoLongMode:
	cli
	lgdt [gdt32_pointer]
	mov eax, cr0
	or eax,0x1
	mov cr0, eax
	mov ax, DATA_SEG
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax

	jmp CODE_SEG:stage2

.LongMode:
    lgdt [GDT64.Pointer]         ; Load the 64-bit global descriptor table.
    jmp GDT64.Code:Realm64       ; Set the code segment and enter 64-bit long mode.

; Access bits
PRESENT        equ 1 << 7
NOT_SYS        equ 1 << 4
EXEC           equ 1 << 3
DC             equ 1 << 2
RW             equ 1 << 1
ACCESSED       equ 1 << 0

; Flags bits
GRAN_4K       equ 1 << 7
SZ_32         equ 1 << 6
LONG_MODE     equ 1 << 5

GDT64:
    .Null: equ $ - GDT
        dq 0
    .Code: equ $ - GDT
        dd 0xFFFF                                   ; Limit & Base (low, bits 0-15)
        db 0                                        ; Base (mid, bits 16-23)
        db PRESENT | NOT_SYS | EXEC | RW            ; Access
        db GRAN_4K | LONG_MODE | 0xF                ; Flags & Limit (high, bits 16-19)
        db 0                                        ; Base (high, bits 24-31)
    .Data: equ $ - GDT
        dd 0xFFFF                                   ; Limit & Base (low, bits 0-15)
        db 0                                        ; Base (mid, bits 16-23)
        db PRESENT | NOT_SYS | RW                   ; Access
        db GRAN_4K | SZ_32 | 0xF                    ; Flags & Limit (high, bits 16-19)
        db 0                                        ; Base (high, bits 24-31)
    .TSS: equ $ - GDT
        dd 0x00000068
        dd 0x00CF8900
    .Pointer:
        dw $ - GDT - 1
        dq GDT

gdt32_start:
	dq 0x0
gdt32_code:
	dw 0xFFFF
	dw 0x0
	db 0x0
	db 10011010b
	db 11001111b
	db 0x0
gdt32_data:
	dw 0xFFFF
	dw 0x0
	db 0x0
	db 10010010b
	db 11001111b
	db 0x0
gdt32_end:
gdt32_pointer:
	dw gdt32_end - gdt32_start
	dd gdt32_start
CODE_SEG equ gdt32_code - gdt32_start
DATA_SEG equ gdt32_data - gdt32_start

bits 16
NoCID: db "CPUID not detected... Entering 32 bit mode.", 0x0A, 0

CheckCPUID:
        ; Check if CPUID is supported by attempting to flip the ID bit (bit 21) in
    ; the FLAGS register. If we can flip it, CPUID is available.

    ; Copy FLAGS in to EAX via stack
    pushfd
    pop eax

    ; Copy to ECX as well for comparing later on
    mov ecx, eax

    ; Flip the ID bit
    xor eax, 1 << 21

    ; Copy EAX to FLAGS via the stack
    push eax
    popfd

    ; Copy FLAGS back to EAX (with the flipped bit if CPUID is supported)
    pushfd
    pop eax

    ; Restore FLAGS from the old version stored in ECX (i.e. flipping the ID bit
    ; back if it was ever flipped).
    push ecx
    popfd

    ; Compare EAX and ECX. If they are equal then that means the bit wasn't
    ; flipped, and CPUID isn't supported.
    xor eax, ecx
    jz .NoCPUID
    ret

.NoCPUID:
    call .NoCPUIDPrintMessage
    jmp 32BitProtectedMode.NoLongMode

.NoCPUIDPrintMessage:
    pushad
    mov esi, NoCID
.PrintLoop:
    lodsb
    test al, al
    je .Done
    mov ah, 0x0E
    int 0x10
    jmp .PrintLoop
.Done:
   popad
   ret

bits 64
Realm64:
    cli                           ; Clear the interrupt flag.
    mov ax, GDT64.Data            ; Set the A-register to the data descriptor.
    mov ds, ax                    ; Set the data segment to the A-register.
    mov es, ax                    ; Set the extra segment to the A-register.
    mov fs, ax                    ; Set the F-segment to the A-register.
    mov gs, ax                    ; Set the G-segment to the A-register.
    mov ss, ax                    ; Set the stack segment to the A-register.
    mov edi, 0xB8000              ; Set the destination index to 0xB8000.
    mov rax, 0x1F201F201F201F20   ; Set the A-register to 0x1F201F201F201F20.
    mov ecx, 500                  ; Set the C-register to 500.
    rep stosq                     ; Clear the screen.
    hlt                           ; Halt the processor.
    jmp 64stage2

times 510 - ($-$$) db 0
dw 0xaa55

target:
bits64
64stage2:
    mov esp,kernel_stack_top
	cli
	extern main
	call main

	jmp $

	hlt

bits 32
32stage2:
    mov esp,kernel_stack_top
	cli
	extern main
	call main

	jmp $

	hlt

__temppointer: dd 0
__tempcont: dd 0

global __e820_size
__e820_size: dd 0
global __e820_data
__e820_data:
	times 20 * 16 db 0

global __drive_count
__drive_count: dd 0
global __drive_data
__drive_data:
	times 12 * 16 db 0

global __vbeInfoBlock
__vbeInfoBlock:
    times 516 db 0
global __vbeModeBlock
__vbeModeBlock:
    times 256 db 0
global __vbeCrtcBlock
__vbeCrtcBlock:
    times 64 db 0
global __biosFontBitmap
__biosFontBitmap:
    dd 0

section .bss
align 4
kernel_stack_bottom: equ $
	resb 65536 ; 16 KB
kernel_stack_top:
