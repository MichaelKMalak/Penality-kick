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

ClearScreen MACRO
        
    ;Clear the screen    
    mov ax, 0600h  ;al=0 => Clear
    mov bh, 07     ;bh=07 => Normal Attributes              
    mov cx, 0      ;From (cl=column, ch=row)
    mov dl, 80     ;To dl=column
    mov dh, 25     ;To dh=row
    int 10h    
    
    ;Move cursor to the beginning of the screen 
    mov ax, 0
    mov ah, 2
    mov dx, 0
    int 10h   
    
ENDM ClearScreen
