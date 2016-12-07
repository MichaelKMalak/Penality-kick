Print MACRO row, column, color    
   mov Ah, 02h
   mov Bh, 0h
   mov Dh, row
   mov Dl, column
   INT 10h 
   mov Ah, 09
   mov Al, ' '
   mov Bl, color
   mov Cx, 1h
   INT 10h 
ENDM Print

Delete Macro row, column
   mov Ah, 02h
   mov Bh, 0h
   mov Dh, row
   mov Dl, column
   int 10h 
   mov Ah, 09
   mov Al, ' '
   mov Bl, 0h
   mov Cx, 1h
   int 10h 
ENDM Delete


PrintText Macro row , column , text
	mov ah,2
	mov bh,0
	mov dl,column
	mov dh,row
	int 10h
	mov ah, 9
    mov dx, offset text
    int 21h
ENDM PrintText
