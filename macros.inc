FlushKeyboardBuffer	MACRO 
;FlushKeyboardBuffer 

mov ah, 0Ch
int 21h
ENDM FlushKeyboardBuffer

HideCursor MACRO
;Hide Cursor 

mov ah, 1
mov cx, 2607h 
int 10h
ENDM HideCursor

Print MACRO row, column, color, char    
  push ax
  push bx
  push cx

  
	SetCursor row, column	

	mov Ah, 09
	mov Al, char
	mov Bl, color
	mov Cx, 1h
	INT 10h  
   

  pop cx
  pop bx
  pop ax
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


PrintStr MACRO Str
   push ax
   push dx
           
        mov ah,9
        mov dx, offset str
        int 21h
		
	pop dx
	pop ax           
ENDM PrintStr 
;----------------------------
PrintChar MACRO 
   push ax
           
        mov ah,2        ;charachter in Dl
        int 21h
		
	pop ax             
ENDM PrintChar 
;----------------------------
ReadStr MACRO Read
   push ax
   push dx
           
        mov ah,0Ah
        mov dx, offset Read
        int 21h
		
  pop dx
  pop ax          
ENDM ReadStr   
;----------------------------
ClearScreen MACRO    
   push ax
                              
           mov ax,0003
           int 10h 
		   
		   HideCursor
	pop ax		   
ENDM ClearScreen   
;----------------------------
SetCursor MACRO Row, Col
   push ax
   push bx
   push dx
           
            mov dh,Row
            mov dl,Col 
            mov bh,00
            mov ah,02
            int 10h
			
	pop dx
	pop bx
	pop ax
ENDM SetCursor  
;----------------------------
ShiftCursorMy MACRO
   LOCAL getOutMy  
   push ax
         
          inc MyCol
          
          cmp MyCol, 79
          jnz getOutMy
          
          mov al,0
          mov MyCol,al
          inc MyRow
          
          cmp MyRow, intialChatRow 
		  
          getOutMy:

	pop ax
ENDM ShiftCursorMy    
;----------------------------
ShiftCursorChat MACRO 
	LOCAL getOutChat
   push ax
             
          inc ChatCol
          
          cmp ChatCol, 79
          jnz getOutChat
          
          mov al,0
          mov ChatCol,al
          inc ChatRow
          
          cmp ChatRow, 26				;CHECK THAT
          jnz getOutChat
          
          call ScrollChatScreen
          dec ChatRow

          getOutChat:
	pop ax
ENDM ShiftCursorChat
;---------------------------- 
PrintHorizontalLine MACRO row, color
   LOCAL loop1
   push cx
                  
    mov cl, 0
    
    loop1:
        Print row, cl, color , ' '      
        inc cl
        cmp cl, 80
    jnz loop1           

	pop cx
ENDM PrintHorizontalLine 
 
;----------------------------
PrintCharAl MACRO row, column ,color

	Print row, column, color, al 

ENDM PrintCharAl
;----------------------------

WriteEnter  MACRO row,column
   push ax
         
                inc row
                mov al, 00h
                mov column,al

  pop ax
ENDM WriteEnter 
;----------------------------  
AddSentToBuffer  MACRO
   push ax
   push bx
              
             
             mov bh,00h
             mov bl, BufferSize
             mov  ah,ToSendChar  
             mov  Buffer[bx],ah
             inc bl
             mov BufferSize,bl  
			 
	pop bx
	pop ax                   
ENDM AddSentToBuffer

;----------------------------
