include snake.inc


.data
	smessage byte "_SNAKE GAME_$",0
	stmessage byte "START$",0
	controls1  byte "     W     ",10,13      ;0Ah,0Dh
			   byte "           ",10,13
			   byte "A         D",10,13
			   byte "           ",10,13
			   byte "     S     $"
    msg byte "Snake Game!!",0
	GameEnd byte "GAME OVER!!!",10,13
			byte "            ",10,13
	        byte "SEMESTER PROJECT BY AHMAD AND SALMAN$",0
    scoremsg byte "Score: ",0
    head byte '^',10,10
    body byte '*',10,11, 3*15 DUP(0)
    segmentcount byte 1
    fruitactive byte 1
    fruitx byte 8
    fruity byte 8
    gameover byte 0
    quit byte 0   
    delaytime byte 5
	left equ 0
	top equ 2
	row equ 15
	col equ 30
	right equ left+col
	bottom equ top+row

.code

gotxy proc
push ax
push bx
mov ah, 02h
mov bh ,0h
int 10h
pop bx
pop ax
ret
gotxy endp 

LastPage proc
	mov ax,@data
	mov ds,ax
	push ax
	push bx
	mov ah, 00h           
	mov al, 13h
	int 10h
	mov dl , 13
	mov dh , 10
	call gotxy
	lea dx, GameEnd
	mov ah,9
	int 21h
	pop bx
	pop ax
	ret
Lastpage endp

FrontPage proc
	mov ax,@data
	mov ds,ax
	push ax
	push bx
	mov ah, 00h           
	mov al, 13h     ;grahics interrupt
	int 10h
	mov dl , 13
	mov dh , 10
	call gotxy
	lea dx, smessage
	mov ah,9
	int 21h
	
	mov dl , 16
	mov dh , 12
	call gotxy
	lea dx , stmessage
	mov ah,9
	int 21h
	
	
	
	
	mov dh, 17
	mov dl, 0
	call gotxy
	lea dx , controls1
	mov ah ,9
	int 21h

	

	pop bx
	pop ax
ret
FrontPage endp

colormap proc
    mov ax,@data
	mov ds,ax
	MOV AX,0600H   
    MOV BH, 00000110b 
    MOV CX,0000H    
    MOV DX,0fffFH   
    INT 10H  
ret	  
colormap endp

main proc ;far
	mov ax, @data
	mov ds, ax 

	mov ax, 0b800H
	mov es, ax
	
	;clearing the screen
	mov ax, 0003H
	int 10H
	
	call FrontPage
	call mouse_D_I

	
	mov ax, 0003H    ;again clearing screen
	int 10H
	call colormap
    call printbox      
    
    
mainloop:       
    call delay 
    call shiftsnake
    cmp gameover,1
    je gameover_mainloop
    
    call keyboardfunctions
    cmp quit, 1
    je quitpressed_mainloop
    call fruitgeneration
    call draw
    
    ;TODO: check gameover and quit
    jmp mainloop
    
gameover_mainloop: 
    mov ax, 0003H
	int 10H
    mov delaytime, 100
    ;mov dx, 0000H
    ;lea bx, gameovermsg
    ;call writestringat
	call LastPage
    call delay    
    jmp quit_mainloop    
     
quitpressed_mainloop:
    mov ax, 0003H
	int 10H    
    mov delaytime, 100
    ;mov dx, 0000H
    ;lea bx, quitmsg
    ;call writestringat
    call LastPage
	call delay    
    jmp quit_mainloop    

quit_mainloop:
;first clear screen
;mov ax, 0003H
;int 10h   
mov dl,0
mov dh,20
call gotxy 
mov ax, 4c00h
int 21h  

delay proc   
    ;this procedure uses 1A interrupt, more info can be found on   
    ;http://www.computing.dcu.ie/~ray/teaching/CA296/notes/8086_bios_and_dos_interrupts.html
    mov ah, 00
    int 1Ah
    mov bx, dx
    
jmp_delay:
    int 1Ah
    sub dx, bx
    ;there are about 18 ticks in a second, 10 ticks are about enough
    cmp dl, delaytime                                                      
    jl jmp_delay    
    ret
    
delay endp
   
   


fruitgeneration proc
    mov ch, fruity
    mov cl, fruitx
regenerate:
    
    cmp fruitactive, 1
    je ret_fruitactive
    mov ah, 00
    int 1Ah
    ;dx contains the ticks
    push dx
    mov ax, dx
    xor dx, dx
    xor bh, bh
    mov bl, row
    dec bl
    div bx
    mov fruity, dl
    inc fruity
    
    
    pop ax
    mov bl, col-1
    dec dl
    xor bh, bh
    xor dx, dx
    div bx
    mov fruitx, dl
    inc fruitx
    
    cmp fruitx, cl
    jne nevermind
    cmp fruity, ch
    jne nevermind
    jmp regenerate             
nevermind:
    mov al, fruitx
    ror al,1
    jc regenerate
    
    
    add fruity, top
    add fruitx, left 
    
    mov dh, fruity
    mov dl, fruitx
    call readcharat
    cmp bl, '*'
    je regenerate
    cmp bl, '^'
    je regenerate
    cmp bl, '<'
    je regenerate
    cmp bl, '>'
    je regenerate
    cmp bl, 'v'
    je regenerate    
    
ret_fruitactive:
    ret
fruitgeneration endp


dispdigit proc
    add dl, '0'
    mov ah, 02H
    int 21H
    ret
dispdigit endp   
   
dispnum proc    
    test ax,ax
    jz retz
    xor dx, dx
    ;ax contains the number to be displayed
    ;bx must contain 10
    mov bx,10
    div bx
    ;dispnum ax first.
    push dx
    call dispnum  
    pop dx
    call dispdigit
    ret
retz:
    mov ah, 02  
    ret    
dispnum endp   

;sets the cursor position, ax and bx used, dh=row, dl = column
;preserves other registers

draw proc

    lea bx, scoremsg
    mov dx, 0102
    call writestringat
    
    
    add dx, 7
    call gotxy
    mov al, segmentcount
    dec al
    xor ah, ah
    call dispnum
        
    lea si, head
draw_loop:
    mov bl, ds:[si]
    test bl, bl
    jz out_draw
    mov dx, ds:[si+1]
    call writecharat
    add si,3   
    jmp draw_loop 

out_draw:
    mov bl, 254
	;mov ah,00h
	;mov al,13h
	;int 10h
    mov dh, fruity
    mov dl, fruitx

    call writecharat
    mov fruitactive, 1
	;mov ax, 1003h
	;mov bx, 0 ; disable blinking.
	;int 10h
	mov dh,dl
    ret
draw endp

;dl contains the ascii character if keypressed, else dl contains 0
;uses dx and ax, preserves other registers
readchar proc
    mov ah, 01H
    int 16H
    jnz keybdpressed
    xor dl, dl
    ret
keybdpressed:
    ;extract the keystroke from the buffer
    mov ah, 00H
    int 16H
    mov dl,al
    ret


readchar endp                    
         
keyboardfunctions proc
    
    call readchar
    cmp dl, 0
    je next_14
    
    ;so a key was pressed, which key was pressed then solti?
    cmp dl, 'w'
    jne next_11
    cmp head, 'v'
    je next_14
    mov head, '^'
    ret
next_11:
    cmp dl, 's'
    jne next_12
    cmp head, '^'
    je next_14
    mov head, 'v'
    ret
next_12:
    cmp dl, 'a'
    jne next_13
    cmp head, '>'
    je next_14
    mov head, '<'
    ret
next_13:
    cmp dl, 'd'
    jne next_14
    cmp head, '<'
    je next_14
    mov head,'>'
next_14:    
    cmp dl, 'q'
    je quit_keyboardfunctions
    ret    
quit_keyboardfunctions:   
    ;conditions for quitting in here please  
    inc quit
    ret
    
keyboardfunctions endp
  
shiftsnake proc     
    mov bx, offset head
    ;determine the where should the head go solti?
    ;preserve the head
    xor ax, ax
    mov al, [bx]
    push ax
    inc bx
    mov ax, [bx]
    inc bx    
    inc bx
    xor cx, cx
l:
    mov si, [bx]
    test si, [bx]
    jz outside
    inc cx     
    inc bx
    mov dx,[bx]
    mov [bx], ax
    mov ax,dx
    inc bx
    inc bx
    jmp l
    
outside:    
    
    
    ;hopefully, the snake will be shifted, i.e. moved.
    ;now shift the head in its proper direction and then clear the last segment. 
    ;But don't clear the last segment if the snake has eaten the fruit
    pop ax
    ;al contains the snake head direction
    
    push dx
    ;dx now consists the coordinates of the last segment, we can use this to clear it
    
    
    lea bx, head
    inc bx
    mov dx, [bx]
    
    cmp al, '<'
    jne next_1
    dec dl
    dec dl
    jmp done_checking_the_head
next_1:
    cmp al, '>'
    jne next_2                
    inc dl 
    inc dl
    jmp done_checking_the_head
    
next_2:
    cmp al, '^'
    jne next_3 
    dec dh               
                   
    
    jmp done_checking_the_head
    
next_3:
    ;must be 'v'
    inc dh
    
done_checking_the_head:    
    mov [bx],dx
    ;dx contains the new position of the head, now check whats in that position   
    call readcharat ;dx
    ;bl contains the result
    
    cmp bl, 254
    je i_ate_fruit
    
    ;if fruit was not eaten, then clear the last segment, 
    ;it will be cleared where?
    mov cx, dx
    pop dx 
    cmp bl, '*'    ;the snake bit itself, gameover
    je game_over
    mov bl, 0
    call writecharat
    mov dx, cx
	;check whether the snake is within the boundary
    cmp dh, top
    je game_over
    cmp dh, bottom
    je game_over
    cmp dl,left
    je game_over
    cmp dl, right
    je game_over

    ;balance the stack, number of segment and the coordinate of the last segment
    
    ret
game_over:
    inc gameover
    ret
i_ate_fruit:    

    ; add a new segment then
    mov al, segmentcount
    xor ah, ah
    
    
    lea bx, body
    mov cx, 3
    mul cx
    
    pop dx
    add bx, ax
    mov byte ptr ds:[bx], '*'
    mov [bx+1], dx
    inc segmentcount 
    mov dh, fruity
    mov dl, fruitx
    mov bl, 0
    call writecharat
    mov fruitactive, 0   
    ret 
shiftsnake endp
   
;Printbox
printbox proc

	;left equ 0
	;top equ 2
	;row equ 15
	;col equ 40
	;right equ left+col
	;bottom equ top+row
;Draw a box around
    mov dh, top
    mov dl, left
    mov cx, col
    mov bl, 205       
	;mov ah, 00h           
	;mov al, 13h
	;int 10h
	call gotxy
l1:
	mov ah ,2
	mov dl ,205
	int 21h
    ;call writecharat
    inc dl
    loop l1
	mov dl,30
    mov dh,2
	call gotxy
	mov ah,2
	mov dl,187
	int 21h
    mov cx, row

l2:
	mov dl,30
    inc dh
	call gotxy
	mov ah,2
	mov dl,186
	int 21h
    loop l2
    mov dl,30
	call gotxy
	mov dl,188
	mov ah,2
	int 21h
    mov cx, col
	mov dl,30
l3:
    dec dl
	push dx
    call gotxy
	mov ah,2
	mov dl,205
	int 21h
	pop dx
	loop l3
	call gotxy
	mov ah,2
	mov dl,200
	int 21h

    mov cx, row     
l4:   
    dec dh
	mov dl,0
	call gotxy
	mov ah,2
	mov dl,186
	int 21h
    loop l4
	mov dl,0
    call gotxy
	mov ah,2
	mov dl,201
	int 21h
    ret
printbox endp
              
;dx contains row, col
;bl contains the character to write
;uses di. 
writecharat proc
    ;80x25
    push dx
    mov ax, dx
    and ax, 0FF00H
	mov al,ah        ;shr 8 :)
	mov ah,0
	
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov es:[di], bl
    pop dx
    ret    
writecharat endp
              
;dx contains row,col
;returns the character at bl
;uses di
readcharat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
	
	mov al,ah
	mov ah,0         ;shr 8 :)  
    
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov bl,es:[di]
    pop dx
    ret
readcharat endp        

;dx contains row, col
;bx contains the offset of the string
writestringat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
 	
	mov al,ah
	mov ah,0     
    
    push bx
    mov bh, 160
    mul bh
    
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
loop_writestringat:
    
    mov al, [bx]
    test al, al
    jz exit_writestringat
    mov es:[di], al
    inc di
    inc di
    inc bx
    jmp loop_writestringat

exit_writestringat:
    pop dx
    ret
writestringat endp


main endp 
end main