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

GetNames    MACRO  
    push ax
    push bx
    push cx
    push dx
    push ds
    
    mov ax, 0600h
    mov bh, 07
    mov cx, 0
    mov dx, 184fh
    int 10h
    
    mov ah, 2
    mov dx, 0
    int 10h
    
    mov ah, 9
    mov dx, offset ask_p1_name
    int 21h
    
    mov ah, 0Ah
    mov dx, offset p1_name
    int 21h
    
    mov ah, 2
    mov dl, 10
    int 21h
    
    mov ah, 2
    mov dl, 13
    int 21h
    
    mov ah, 9
    mov dx, offset ask_p2_name
    int 21h
    
    mov ah, 0Ah
    mov dx, offset p2_name
    int 21h
      
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax  
     
ENDM    GetNames    


DrawInterface   MACRO
    
    push ax
    push bx
    push cx
    push dx
    push ds
    
    mov ax, 0600h
    mov bh, 07
    mov cx, 0
    mov dx, 184fh
    int 10h
    
    
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0
        
    ;Draw + (Player)
    mov ah, 2
    mov dl, 5 ;Move to position X=5
    mov dh, 7 ;Move to position Y=7
    int 10h  
    
    mov dh, 0
    mov ah, 2
    mov dl, '+'
    int 21h
    
    ;Write P1 or P2 under the + sign
    mov al, 0
    mov ah, 2
    mov dl, 5 ;Move to position X=5
    mov dh, 8 ;Move to position Y=11
    int 10h 
    
    mov dh, 0
    mov ah, 2
    mov dl, 'P'
    int 21h
                            
    cmp current_player, 1  ;Check if the current player is player1
    jz write_1
    
    ;if player 2 => Print 2
    mov dh, 0
    mov ah, 2
    mov dl, '2'
    int 21h
    
    mov ax,0
    AND ax,ax
    jz continue1
    
    ;if player 1 => Print 1
    write_1:
    mov dh, 0
    mov ah, 2
    mov dl, '1'
    int 21h
    
    continue1:
    
    ;Draw the goal
    
        ;Draw colored spaces indicating the goal beginning
        
        mov cl, GoalDim[0]
        
        
        loop0:
        push cx
        Print GoalDim[1], cl, 0c0h
        pop cx
                
        inc cl  
        mov bl, GoalDim[2]
        inc bl
        cmp cl, bl 
        jnz loop0 
        
        
        ;Draw the back of the goal (|)
        mov cl, GoalDim[1]
        inc cl 
        loop1:
            push cx   
            Print cl, GoalDim[2], 0c0h   
            pop cx
            inc cl
            cmp cl, GoalDim[3]
        jnz loop1
        
        
        ;Draw colored spaces indicating the goal ending
        mov cl, GoalDim[2]
        
        loop2:
        push cx
        Print GoalDim[3], cl, 0c0h
        pop cx
         
        dec cl
        mov bl, GoalDim[0]
        dec bl
        cmp cl, bl        
        jnz loop2 
        
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
                   
ENDM DrawInterface


WriteOneFifthScreen MACRO
    push ax
    push bx
    push cx
    push dx
    push ds
mov ax, 0
mov bx, 0
mov cx, 0
mov dx, 0

;Draw last one fifth section
    
    ;Draw Horizontal Line
    
    loop3:
        push cx
        Print 15, cl, Hr_Line_Color      
        pop cx      
        inc cl
        cmp cl, 80
    jnz loop3
    
    ;Move to the next line
    
    mov ah, 2      
    mov dl, 0 ;Move to position X=0
    mov dh, 16 ;Move to position Y=21
    int 10h 
    
    
    ;Writing players' names and scores
    
    ;Player 1 Info  
    
    push bx
    push cx
    mov cx, 0 
    mov bx, 2
    mov cl, p1_name[1]
    p1_loop:
    mov ah,2
    mov dl, p1_name[bx]
    int 21h
    inc bx
    loop p1_loop
    pop cx
    pop bx
       
    mov ah, 2
    mov bx, 0
    mov dl, ':'
    int 21h
    
    
    mov bl, p1_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Move to write player 2 info
    mov ah, 2      
    mov dh, 16 ;Move to position Y=16 
    mov dl, 78 
    sub dl, p1_name[1]
    int 10h
    
    ;Player 2 Info
   
    push bx
    push cx 
    mov bx, 2
    mov cx, 0
    mov cl, p2_name[1]
    p2_loop:
    mov ah,2
    mov dl, p2_name[bx] 
    int 21h
    inc bx
    loop p2_loop
    pop cx
    pop bx
    
    mov ah, 2
    mov dl, ':'
    int 21h
           
    mov bl, p2_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h  
    
        
    ;Draw Horizontal Line
    mov cx, 0
    
    loop6:               
        push cx
        Print 17, cl, Hr_Line_Color      
        pop cx
        inc cl
        cmp cl, 80
    jnz loop6
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax

    ENDM WriteOneFifthScreen


