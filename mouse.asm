INCLUDE snake.inc

.code

	mouse_D_I PROC
		push ax
		mov ax,0001h
		int 33h        ;showing cursor on screen

		l:                  
			mov ax, 0005h   ;button status
			mov bx, 0        
			int 33h
		
		cmp ax,1
		jne l
		pop ax
		ret
	mouse_D_I ENDP

END