section .data               
;Canviar Nom i Cognom per les vostres dades.
developer db "_Jordi_ _Felip_",0

;Constants que també estan definides en C.
ROWDIM  equ 4       ;files de la matriu.
COLDIM  equ 5       ;columnes de la matriu.

section .text            

;Variables definides en Assemblador.
global developer  

;Subrutines d'assemblador que es criden des de C.
global posCurScreenP1, showDigitsP1, updateBoardP1,
global calcIndexP1, moveCursorP1, openCardP1
global playP1

;Variables definides en C.
extern charac, row, col, indexMat, rowcol, rowScreen, colScreen
extern value, moves, pairs, mCards, mOpenCards, state

;Funcions de C que es criden des de assemblador
extern clearScreen_C, printBoardP1_C, gotoxyP1_C, getchP1_C, printchP1_C
extern printMessageP1_C


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATENCIÓ: 
;;   Recordeu que les variables i els paràmetres s'han d'assignar
;;   al tipus de registre que correspon en cada cas:
;;   Variables de tipus 'char', en assemblador s'han d'assignar a 
;;   registres de tipus BYTE (1 byte): 
;;   al, ah, bl, bh, cl, ch, dl, dh, sil, dil, r8b, ..., r15b.
;;   Variables de tipus 'short', en assemblador s'han d'assignar a 
;;   registres de tipus WORD (2 byte): 
;;   ax, bx, cx, dx, si, di, r8w, ..., r15w.
;;   Variables de tipus 'int' en assemblador s'han d'assignar a 
;;   registres de tipus DWORD (4 byte): 
;;   deax, ebx, ecx, edx, esi, edi, r8d, ..., r15d.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Les subrutines en assemblador que heu d'implementar són:
;;   showNumberP1, updateBoardP1,
;;   moveCursorP1, calcIndexP1, openCardP1.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Aquesta subrutina es dóna feta. NO LA PODEU MODIFICAR.
; Situar el cursor a la fila indicada per la variable (rowScreen) i a 
; la columna indicada per la variable (colScreen) de la pantalla,
; cridant la funció gotoxyP1_C.
; 
; Variables globals utilitzades:   
; rowScreen: fila de la pantalla on posicionem el cursor.
; colScreen: columna de la pantalla on posicionem el cursor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gotoxyP1:
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ;apuntador de la pila; conté sempre el primer element de la pila
   
   push rax	
   push rbx 
   push rcx 
   push rdx 
   push rsi 
   push rdi 
   push r8 
   push r9 
   push r10 
   push r11 
   push r12 
   push r13 
   push r14 
   push r15 

   call gotoxyP1_C
 
   pop r15 
   pop r14 
   pop r13 
   pop r12 
   pop r11 
   pop r10 
   pop r9 
   pop r8 
   pop rdi 
   pop rsi 
   pop rdx 
   pop rcx 
   pop rbx 
   pop rax 

   mov rsp, rbp
   pop rbp
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Aquesta subrutina es dóna feta. NO LA PODEU MODIFICAR.
; Mostrar un caràcter guardat a la variable (charac) a la pantalla, 
; en la posició on està el cursor, cridant la funció printchP1_C
; 
; Variables globals utilitzades:   
; charac   : caràcter que volem mostrar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printchP1:
   push rbp
   mov  rbp, rsp
   
   push rax 
   push rbx 
   push rcx 
   push rdx 
   push rsi 
   push rdi 
   push r8 
   push r9 
   push r10 
   push r11 
   push r12 
   push r13 
   push r14 
   push r15 

   call printchP1_C
 
   pop r15 
   pop r14 
   pop r13 
   pop r12 
   pop r11 
   pop r10 
   pop r9 
   pop r8 
   pop rdi 
   pop rsi 
   pop rdx 
   pop rcx 
   pop rbx 
   pop rax 

   mov rsp, rbp
   pop rbp
   ret
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Aquesta subrutina es dóna feta. NO LA PODEU MODIFICAR.
; Llegir una tecla i guarda el caràcter associat a la variable (charac)
; sense mostrar-la per pantalla, cridant la funció getchP1_C. 
; 
; Variables globals utilitzades:   
; charac   : caràcter que llegim de teclat.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getchP1:
   push rbp
   mov  rbp, rsp
   
   push rax 
   push rbx  
   push rcx 
   push rdx 
   push rsi 
   push rdi 
   push r8 
   push r9 
   push r10 
   push r11 
   push r12 
   push r13 
   push r14 
   push r15 
   push rbp 

   call getchP1_C
 
   pop rbp 
   pop r15 
   pop r14 
   pop r13 
   pop r12 
   pop r11 
   pop r10 
   pop r9 
   pop r8 
   pop rdi 
   pop rsi 
   pop rdx 
   pop rcx 
   pop rbx 
   pop rax 
   
   mov rsp, rbp
   pop rbp
   ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (VE IMPLEMENTADA)
; Posicionar el cursor a la pantalla, dins el tauler, en funció de la
; posició del cursor dins la matriu, indicada pel vector (rowcol) 
; rowcol[0]:fila (0-3) i rowcol[1]:columna (0-4), de tipus short(WORD)2bytes
; a partir de la posició [10, 12] de la pantalla.
; Per a calcular la posició del cursor a pantalla (rowScreen) i 
; (colScreen) utilitzar aquestes fórmules:
; rowScreen=10+(rowcol[0]*2)
; colScreen=12+(rowcol[1]*4)
; Per a posicionar el cursor a la pantalla s'ha de cridar a la subrutina gotoxyP1.
;
; Variables globals utilitzades:	
; rowcol     : Vector on tenim la posició del cursor a la pantalla.
; rowScreen  : Fila on volem posicionar el cursor a la pantalla.
; colScreen  : Columna on volem posicionar el cursor a la pantalla.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
posCurScreenP1:
   push rbp  ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp
   
   push rax ;emmagatzema el registre a la pila
   push rbx ;emmagatzema el registre a la pila
   
   mov rax, 0
   mov ax, WORD[rowcol+0] ; s'assigna a ax el valor de la fila
   shl ax, 1        ;(rowcol[0]*2) desplaçament lògic a l'esquerra 
   add ax, 10       ;rowScreen=10+(rowcol[0]*2);
   
   mov rbx, 0
   mov bx, WORD[rowcol+2]   ; s'assigna a bx el valor de la columna
   shl bx, 2        ;(rowcol[1]*4) desplaçament lògic a l'esquerra 
   add bx, 12       ;colScreen=12+(rowcol[1]*4);
   
   mov DWORD[rowScreen], eax ; s'utilitza eax ja que es rowScreen es un INT 
   mov DWORD[colScreen], ebx ; s'utilitza ebx ja que es rowScreen es un INT 
   call gotoxyP1    ;gotoxyP1_C();
   
   posCurScreenP1_End:
   pop rbx ;treu el registre del cim de la pila
   pop rax ;treu el registre del cim de la pila
   
   mov rsp, rbp
   pop rbp
   ret



;;;;;
; Converteix un valor (value) de tipus int(4 bytes) (entre 0 i 99) en  
; dos caràcters ASCII que representin aquest valor. (27 -> '2' '7').

; S'ha de dividir el valor entre 10, el quocient representarà les 
; desenes i el residu les unitats, i després s'han de convertir a ASCII
; sumant '0' o 48(codi ASCII de '0') a les unitats i a les desenes.
; Mostra els dígits (caràcter ASCII) a partir de la fila indicada
; per la variable (rowScreen) i a la columna indicada per la variable
; (colScreen).
; Per a posicionar el cursor es cridar a la subrutina gotoxyP1 i per a 
; mostrar els caràcters a la subrutina printchP1.
; 
; Variables globals utilitzades:	
; rowScreen: Fila de la pantalla on posicionem el cursor.
; colScreen: Columna de la pantalla on posicionem el cursor.
; charac   : Caràcter que llegim de teclat.
; value    : Valor que volem mostrar.
;;;;;
showDigitsP1:
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp
   
   push rax  ;emmagatzema el registre a la pila
   push rbx	 ;emmagatzema el registre a la pila
   push rdx  ;emmagatzema el registre a la pila
   
   mov rax, 0
   mov eax, DWORD[value] ; guardem al registre eax el valor que volem mostrar
   
   mov edx, 0
   mov ebx, 10
   div ebx ; operand font es de 4 bytes -> (32bits) -> EAX = EDX:EAX / font, EDX = EDX:EAX mod font -> EAX=EDX:EAX/EBX  EDX=EDX:EAX%EBX
   
   add al,'0'      ;convertim els valors a caràcters ASCII -> QUOCIENT
   add dl,'0'      ;charac = charac + '0'; -> RESTA
   
   call gotoxyP1 ;Situar el cursor a la fila indicada per la variable (rowScreen) i a la columna indicada per la variable (colScreen) de la pantalla
	
   mov BYTE[charac],dl ; movem el caracter a imprimir a la variable (charac) per cridar printchP1
   
   ; UNITATS
   call printchP1 ; Mostrar un caràcter guardat a la variable (charac) a la pantalla, en la posició on està el cursor
   

   dec DWORD[colScreen]    ; decrementem per tal de poder imprimir les desenes
   
   call gotoxyP1 ;Situar el cursor a la fila indicada per la variable (rowScreen) i a la columna indicada per la variable (colScreen) de la pantalla
   
   mov BYTE[charac], al ; movem el caracter a imprimir a la variable (charac) per cridar printchP1
   
   ; DESENES
   call printchP1 ; Mostrar un caràcter guardat a la variable (charac) a la pantalla, en la posició on està el cursor
   
   pop rax ;treu el registre del cim de la pila
   pop rbx ;treu el registre del cim de la pila
   pop rdx ;treu el registre del cim de la pila
   
   mov rsp, rbp
   pop rbp
   ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mostrar els valors de la matriu (mOpenCards) dins el tauler, 
; a les posicions corresponents, els moviments (moves) i les parelles 
; fetes (pairs). 
; S'ha de recórrer tota la matriu (mOpenCards), cada posició és de 
; tipus char(BYTE)1byte, i per a cada element de la matriu fer:
; Posicionar el cursor en el tauler en funció de les variables 
; (rowScreen) fila i (colScreen) columna cridant la subrutina gotoxyP1.
; Les variables (rowScreen) i (colScreen) s'inicialitzaran a, a 10 i 14
; respectivament, que és la posició a pantalla de la casella [0][0].
; Mostrar els caràcters de cada posició de la matriu (mOpenCards) 
; cridant la subrutina printchP1.
; Després, mostrar els moviments (moves) de tipus int(DWORD)4bytes, 
; a partir de la posició [19,15] de la pantalla i mostrar les parelles
; fetes (pairs) de tipus int(DWORD)4bytes, a partir de la 
; posició [19,24] de la pantalla cridant la subrutina showDigitsP1.
; 
; Variables globals utilitzades:			
; rowScreen  : Fila on volem posicionar el cursor a la pantalla.
; colScreen  : Columna on volem posicionar el cursor a la pantalla.
; mOpenCards : Matriu on guardem les targetes del joc.
; charac     : Caràcter que volem mostrar.
; value      : Valor que volem mostrar.
; moves      : Parelles que s'han intentat fer amb èxit o sense.
; pairs      : Parelles que s'han fet.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
updateBoardP1:
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp
   
   push rax ;emmagatzema el registre a la pila 63-0
   push rbx ;emmagatzema el registre a la pila 63-0
   push rcx ;emmagatzema el registre a la pila 63-0
   push rdx ;emmagatzema el registre a la pila 63-0
         
   mov edx,0 ;index per accedir a la matriu mOpenCards
   
   mov r8d, DWORD[pairs] ; estan inicialitzats a 0
   mov r9d, DWORD[moves] ; estan inicialitzats a 0
   
   mov DWORD[rowScreen], 10 ;inicialització de la variable rowScreen
   
   updateBoardP1_for1:            
		cmp ebx, ROWDIM ; comparació amb el numero de files; 31-0
		jge updateBoardP1_endfor1	; ja haurem recorregut tota la matriu
		mov DWORD[colScreen], 12 ;inicialitzacio de la variable colScreen
		mov ecx, 0 ;columnes

		updateBoardP1_for2: 
			cmp ecx, COLDIM ; comparació amb el numero de files; 31-0
			jge updateBoardP1_endfor2 ; cas que haguem recorregut totes les columnes de la fila;
			
			call gotoxyP1          ;gotoxyP1_C(); ; Situar el cursor a la fila indicada per la variable (rowScreen) i a  la columna indicada per la variable (colScreen) de la pantalla
			
			mov al, BYTE[mOpenCards+edx] ;charac = mOpenCards[i][j]; 
			mov BYTE[charac], al  ; moc el valor de la X a charac
			call printchP1         ;printchP1_C(); -> Mostrar un caràcter guardat a la variable (charac) a la pantalla
			
			add DWORD[colScreen], 4 ;colScreen = colScreen + 4
			inc edx                ;incrementem l'índex per a accedir a la matriu mOpenCards		
			inc ecx                ;Actualitzem la columna.
			
			jmp updateBoardP1_for2
		
		updateBoardP1_endfor2:
		
		add DWORD[rowScreen], 2   ;rowScreen = rowScreen + 2;	
		inc ebx                    ;Actualitzem la fila.
		jmp updateBoardP1_for1
	updateBoardP1_endfor1:
   updateBoardP1_end:
		
   mov DWORD[rowScreen], 19 ;rowScreen=19;
   mov DWORD[colScreen], 15 ;colScreen=15;
  
   mov DWORD[value], r9d ;value = moves; 
   
   call showDigitsP1 ;showDigitsP1_C();
   
   mov DWORD[colScreen],24 ;colScreen = 24;
   
   mov DWORD[value], r8d ;value = pairs;
  
   call showDigitsP1 ;showDigitsP1_C();
   
   pop rdx ;treu el registre del cim de la pila
   pop rcx ;treu el registre del cim de la pila
   pop rbx ;treu el registre del cim de la pila
   pop rax ;treu el registre del cim de la pila
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Actualitzar la posició del cursor dins la matriu actualitzant el 
; vector (rowcol):vector de tipus short(WORD)2bytes, amb la fila 
; rowcol[0] i la columna rowcol[1] de la posició del cursor dins 
; la matriu, en funció de la tecla premuda que tenim a la 
; variable (charac) de tipus char(BYTE)1byte,
; (i: amunt, j:esquerra, k:avall, l:dreta).
; Comprovar que no sortim de la matriu, el vector (rowcol) només
; pot prendre els valors de les posicions dins de la matriu.
; Les files i les columnes s'incrementen de 1 en 1 perquè cada
; posició de la matriu és de tipus char(BYTE)1byte.
; Si s'ha de sortir de la matriu, no fer el moviment.
; No s'ha de posicionar el cursor a la pantalla subrutina posCurScreenP1.
; 
; Variables globals utilitzades:	
; charac : Caràcter que llegim de teclat.
; rowcol : Vector per a indicar la posició del cursor a la matriu.
;;;;;
moveCursorP1:  
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp
   
   cmp BYTE[charac],'i' ;case 'i' // amunt 
   je moveCursorP1_Up
   cmp BYTE[charac],'j' ;case 'j' // esquerra
   je moveCursorP1_Left
   cmp BYTE[charac],'k' ;case 'k' // avall 
   je moveCursorP1_Down    
   cmp BYTE[charac],'l' ;case 'l' // dreta
   je moveCursorP1_Right
		
	moveCursorP1_Up:
		cmp WORD[rowcol+0], 0     ;if (rowcol[0]>0) 
		jle moveCursorP1_End  
		dec WORD[rowcol+0]        ;rowcol[0]--;
		jmp moveCursorP1_End  ; saltem al final

	moveCursorP1_Down:
		cmp WORD[rowcol+0], ROWDIM-1  ; comparem el valor de la fila amb el limit inferior
		jge moveCursorP1_End  ; si es mes gran o igual evitem fer el moviment
		inc WORD[rowcol+0] ;rowcol[0]++;
		jmp moveCursorP1_End ; saltem al final

	moveCursorP1_Left:
		cmp WORD[rowcol+2], 0  ; comparem el valor de la columna amb el limit de l'esquerre (0)
		jle  moveCursorP1_End ; evitem fer el moviment si es igual o mes petit (saltem al final)
		dec WORD[rowcol+2]  ;rowcol[1]--;
		jmp moveCursorP1_End  ; saltem al final

	moveCursorP1_Right:
		cmp WORD[rowcol+2], COLDIM-1 ; comparem el valor de la columna amb el limit de la dreta
		jge moveCursorP1_End ;evitem fer el moviment si es igual o mes gran (saltem al final)
		inc WORD[rowcol+2]  ;rowcol[1]++;
		jmp moveCursorP1_End ; saltem al final
		   
   moveCursorP1_End:
         
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Calcular el valor de l'índex (indexMat) per a accedir a una matriu 
; (4x5) de ROWDIM * COLDIM posicions de tipus char(BYTE)1byte, cadascuna,
; a partir de la fila (row) i la columna (col) especificades.
; indexMat=((row*COLDIM)+(col))
; m[i][j] en C, és equivalent a BYTE[m+eax] en assemblador, 
; si eax = indexMat = ((row*COLDIM)+(col)). 
; m[1][2] en C, és DWORD[m+7] en assemblador.
;
; Aquesta subrutina no té una funció en C equivalent.
;
; Variables globals utilitzades:   
; rowcol  : Vector per a indicar la posició del cursor dins la matriu.
; indexMat: Índex per a accedir a matrius (4x5).
;;;;;  
calcIndexP1:
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp

   push rax ;emmagatzema el registre a la pila
   push rbx ;emmagatzema el registre a la pila
   push rdx ;emmagatzema el registre a la pila
   
   mov rax, 0 ; inicialitzem el registre a 0
   mov rbx, 0 ; inicialitzem el registre a 0
   mov rdx, 0 ; inicialitzem el registre a 0
   
   mov ax, WORD[rowcol+0] ; guardem el valor de row a ax
   mov ebx, COLDIM ; guardem el valor de COLDIM a ebx
   mul ebx ; fem la multiplicacio de eax * ebx ->(row*COLDIM) 
   add ax, WORD[rowcol+2] ; fem la suma amb el valor de col
   mov DWORD[indexMat], eax ; movem el valor a la variable de l'index de la matriu 
   
   calcIndexP1_End:
   pop rdx ;treu el registre del cim de la pila
   pop rbx ;treu el registre del cim de la pila
   pop rax ;treu el registre del cim de la pila
          
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Obrir la targeta de la matriu (mCards) de la posició indicada pel
; cursor dins la matriu i que tenim al vector (rowcol). 
; rowcol[0] fila i rowcol[1] columna.
; Si la targeta no està girada (!='x') posar-la a la matriu (mOpenCards)
; per a que es mostri.
; Marcar-la amb una 'x'(minúscula) a la mateixa posició la matriu 
; (mCards) per a saber que està girada.
; Passar al següent estat (state++)
; 
; No s'ha de mostrar la matriu amb els canvis, es fa a updateBoardP1.
; 
; Variables globals utilitzades:
; rowcol     : Vector on tenim la posició del cursor dins la matriu.
; mCards     : Matriu on guardem les targetes del joc.
; mOpenCards : Matriu on tenim les targetes obertes del joc.
; state      : Estat del joc.
;;;;;
openCardP1:  
   push rbp ; Emmagatzemar el registre rbp a la pila
   mov  rbp, rsp ; Assignar a rbp el valor del registre apuntador rsp
   
   push rax ;emmagatzema el registre a la pila
   push rbx ;emmagatzema el registre a la pila
   
   call calcIndexP1 ; cridem la funcio de calcIndexP1 per saber el valor de l'índex (indexMat) per a accedir a una matriu 
   mov esi,DWORD[indexMat] ; movem el valor del valor de l'index al registre esi
   
   mov r8b, BYTE[mCards+esi] ; accedim al valor de l'index de la matriu mCards -> charac = cards[i][j]; matriu amb valors
   mov r9b, BYTE[mOpenCards+esi] ; accedim al valor de l'index de la matriu mOpenCards -> matriu amb X
   
   cmp r8b,'x' 			;mCards[i][j] != 'x' ; comparem el valor de la matriu mcards amb 'x', '0' != 'x';
   je openCardP1_End 	;salta si son iguals;
   
   mov BYTE[mOpenCards+esi], r8b ;mOpenCards[i][j] = mCards[i][j];
   mov BYTE[mCards+esi],'x' ;mCards[i][j] = 'x';
      
   inc DWORD[state] ;state++
   
   openCardP1_End:
   pop rbx
   pop rax
   
         
   mov rsp, rbp
   pop rbp
   ret


;;;;;
; Aquesta subrutina es dóna feta. NO LA PODEU MODIFICAR.
; Mostra un missatge a sota del tauler segons el valor de la variable 
; (state) cridant la funció printMessageP1_C.
; (state) 0: 0 targetes obertes.
;         1: 1 Targeta oberta.
;         2: 2 Targetes obertes.
;         5: Sortir, hem premut la tecla 'ESC' per a sortir.
; Si (state>1) demana que es premi una tecla per a poder-lo llegir.
;  
; Variables globals utilitzades:	
; state  : Estat del joc.
; rowScreen: Fila de la pantalla on posicionem el cursor.
; colScreen: Columna de la pantalla on posicionem el cursor. 
;;;;;
printMessageP1:
   push rbp
   mov  rbp, rsp
   ;guardem l'estat dels registres del processador perquè
   ;les funcions de C no mantenen l'estat dels registres.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   ; Quan cridem la funció printMessageP1_C() des d'assemblador, 
   call printMessageP1_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret

;;;;;
; Joc del Memory
; Subrutina principal del joc.
; Trobat totes les parelles del tauler (10 parelles), girant les
; targetes de dues en dues. Com a màxim es poden fer 15 intents.
; 
; Pseudo-codi:
; Inicialitzar l'estat del joc, (state=0).
; Esborrar la pantalla  (cridar la funció clearScreen_C).
; Mostrar el tauler de joc (cridar la subrutina printBoardP1).
; Actualitzar el tauler de joc i els valors dels moviments fets (moves)
; i de les parelles fetes (pairs) cridant la subrutina updateBoardP1.
; Mentre (state<3) fer:
;   Mostrar un missatge,  segons el valor de la variable (state),
;   per a indicar que s'ha de fer, cridant la subrutina printMessageP1.
;   Actualitzar la posició del cursor a la pantalla a partir del vector
;   ([rowcol]) (fila (rowcol[0]) i la columna (rowcol[1])) amb la posicio
;   del cursor dins la matriu, cridant la subrutina posCurScreenP1.
;   Llegir una tecla, cridar la subrutina getchP1. 
;   Segons la tecla llegida cridarem a les subrutines que corresponguin.
;     - ['i','j','k' o 'l'] desplaçar el cursor segons la direcció 
;       triada, cridant la subrutina moveCursorP1).
;     - '<SPACE>'(codi ASCII 32) girar la targeta on hi ha el cursor
;       cridant la funció openCardP1_C.
;       Actualitzar el tauler de joc i els valors dels moviments fets (moves)
;       i de les parelles fetes (pairs) cridant la subrutina updateBoardP1.
;       Si s'han girat dues targetes (state>1) incrementar el moviments (moves)
;       i posar (state=0) per a tornar a fer una nova parella.
;    - '<ESC>'  (codi ASCII 27) posar (state = 5) per a sortir.
;       No sortira si només s'ha girat una targeta (state!=1).
; Fi mentre.
; Sortir: S'acaba el joc.
; 
; Variables globals utilitzades:	
; rowcol     : Vector on tenim la posició del cursor dins la matriu.
; state      : Indica l'estat del joc. 0:sortir, 1:jugar.
; charac     : Caràcter que llegim de teclat.
;;;;;  
playP1:
   push rbp
   mov  rbp, rsp
   
   push rax
   
   mov DWORD[state], 0   ;state = 0;//Estat per a començar a jugar
   mov DWORD[moves], 0   ;moves = 0;//Parelles que s'han intentat fer amb èxit o sense.
   mov DWORD[pairs], 0   ;pairs = 0;//Parelles que s'han fet.  
   mov WORD[rowcol+0], 2 ;rowcol[0] = 2; //Posició inicial del cursor dins la matriu.
   mov WORD[rowcol+2], 1 ;rowcol[1] = 1;
   
   call clearScreen_C
   call printBoardP1_C        ;printBoard1_C();
     
   call updateBoardP1

   playP1_Loop:               ;while  {  //Bucle principal.
   cmp  DWORD[state], 3        ;(state < 3)
   jge  playP1_End
      
      call printMessageP1     ;printMessageP1_C();
      call posCurScreenP1     ;posCurScreenP1_C();     
      call getchP1            ;charac = getchP1_C();   
      mov  al, BYTE[charac]
      
      cmp al, 'i'             ;if (charac>='i' && charac<='l') {
      jl  playP1_TurnUp
      cmp al, 'l'
      jg  playP1_TurnUp
         call moveCursorP1    ;moveCursorP1_C();
      playP1_TurnUp:
      cmp al, 32              ;if (charac==32) {
      jne playP1_Esc
         call openCardP1      ;state = openCardP1_C();
         
         cmp DWORD[state], 1           ;if (state > 1) {
         jle playP1_Update
			inc DWORD[moves]  ;moves++;
			mov DWORD[state],0;state = 0;
	     playP1_Update:
         call updateBoardP1   ;updateBoardP1_C();
      playP1_Esc:
      cmp al, 27              ;if ( (charac==27) && (state!=1) ) { 27 en ascii es escape
      jne playP1_EndLoop
      cmp DWORD[state], 1
      je  playP1_EndLoop
         mov DWORD[state], 5  ;state = 5;
      playP1_EndLoop:
   jmp playP1_Loop
   
   playP1_End:
   pop rax  
   
   mov rsp, rbp
   pop rbp
   ret
