/**
 * Implementació en C de la pràctica, per a què tingueu una
 * versió funcional en alt nivell de totes les funcions que heu 
 * d'implementar en assemblador.
 * Des d'aquest codi es fan les crides a les subrutines de assemblador. 
 * AQUEST CODI NO ES POT MODIFICAR I NO S'HA DE LLIURAR.
 **/
 
#include <stdio.h>
#include <termios.h>    //termios, TCSANOW, ECHO, ICANON
#include <unistd.h>     //STDIN_FILENO

/**
 * Constants
 */
#define ROWDIM  4        //files de la matriu
#define COLDIM  5        //columnes de la matriu

extern int developer;//Variable declarada en assemblador que indica el nom del programador

/**
 * Definició de variables globals
 */
char  charac;     //Caràcter llegit de teclat i per a escriure a pantalla.

int   row;        //Índex per a accedir a les matrius.
int   col;        //Columna per a accedir a les matrius
int   indexMat;   //Índex per a accedir a les matrius en assemblador (index=(row*ROWDIM)+col [0..(ROWDIM*COLDIM)-1].
short rowcol[2];  //Vector de dues posicions de tipus short(2 bytes) on  
                  //tenim la fila (rowcol[0]) i la columna (rowcol[1]) 
                  //per a indicar la posició del cursor dins la matriu.
int   rowScreen;  //Fila on volem posicionar el cursor a la pantalla.
int   colScreen;  //Columna on volem posicionar el cursor a la pantalla.
                  
int   value;      //Valor que convertim a caràcter
int   moves;      //Parelles que s'han intentat fer amb èxit o sense.
int   pairs;      //Parelles que s'han fet.


// Matriu 4x5 amb les targetes del joc.
char mCards[ROWDIM][COLDIM]     = { 
	              {'0','1','2','3','4'},
                  {'S','@','O','#','$'},
                  {'S','#','O','@','$'},
                  {'4','3','2','1','0'} };  

// Matriu 4x5 amb les targetes obertes.       
char mOpenCards[ROWDIM][COLDIM] = { 
	              {'X','X','X','X','X'},
                  {'X','X','X','X','X'},
                  {'X','X','X','X','X'},
                  {'X','X','X','X','X'} };

int state  = 0;// 0: 0 Targetes obertes.
               // 1: 1 Targeta oberta.
               // 2: 2 Targetes obertes.
               // 5: Sortir, hem premut la tecla 'ESC' per a sortir.


/**
 * Definició de les funcions de C
 */
void clearscreen_C();
void gotoxyP1_C();
void printchP1_C();
void getchP1_C();

void printMenuP1_C();
void printBoardP1_C();

void posCurScreenP1_C();
void showDigitsP1_C();
void updateBoardP1_C();
void moveCursorP1_C();
void openCardP1_C();

void printMessageP1_C();
void playP1_C();


/**
 * Definició de les subrutines d'assemblador que es criden des de C.
 */
void posCurScreenP1();
void showDigitsP1();
void updateBoardP1();
void moveCursorP1();
void calcIndexP1();
void openCardP1();
void checkPairsP1_C();
void playP1();


/**
 * Esborrar la pantalla
 * 
 * Variables globals utilitzades:   
 * Cap
 * 
 * Aquesta funció no es crida des d'assemblador
 * i no hi ha definida una subrutina d'assemblador equivalent.
 */
void clearScreen_C(){
   
    printf("\x1B[2J");
    
}


/**
 * Situar el cursor a la fila indicada per la variable (rowScreen) i a 
 * la columna indicada per la variable (colScreen) de la pantalla.
 * 
 * Variables globals utilitzades:   
 * rowScreen: Fila de la pantalla on posicionem el cursor.
 * colScreen: Columna de la pantalla on posicionem el cursor.
 * 
 * S'ha definit un subrutina en assemblador equivalent 'gotoxyP1' per a 
 * poder cridar aquesta funció guardant l'estat dels registres del 
 * processador.
 * Això es fa perquè les funcions de C no mantenen l'estat dels registres.
 */
void gotoxyP1_C(){
   
   printf("\x1B[%d;%dH",rowScreen,colScreen);
   
}


/**
 * Mostrar un caràcter guardat a la variable (charac) a la pantalla, 
 * en la posició on està el cursor.
 * 
 * Variables globals utilitzades:   
 * charac   : Caràcter que volem mostrar.
 * 
 * S'ha definit un subrutina en assemblador equivalent 'printchP1' per a
 * cridar aquesta funció guardant l'estat dels registres del processador.
 * Això es fa perquè les funcions de C no mantenen l'estat dels registres.
 */
void printchP1_C(){

   printf("%c",charac);
   
}


/**
 * Llegir una tecla i guarda el caràcter associat a la variable (charac)
 * sense mostrar-lo per pantalla. 
 * 
 * Variables globals utilitzades:   
 * charac   : Caràcter que llegim de teclat.
 * 
 * S'ha definit un subrutina en assemblador equivalent 'getchP1' per a
 * cridar aquesta funció guardant l'estat dels registres del processador.
 * Això es fa perquè les funcions de C no mantenen l'estat dels 
 * registres.
 */
void getchP1_C(){

   static struct termios oldt, newt;

   /*tcgetattr obtenir els paràmetres del terminal
   STDIN_FILENO indica que s'escriguin els paràmetres de l'entrada estàndard (STDIN) sobre oldt*/
   tcgetattr( STDIN_FILENO, &oldt);
   /*es copien els paràmetres*/
   newt = oldt;

   /* ~ICANON per a tractar l'entrada de teclat caràcter a caràcter no com a línia sencera acabada amb /n
      ~ECHO per a què no mostri el caràcter llegit*/
   newt.c_lflag &= ~(ICANON | ECHO);          

   /*Fixar els nous paràmetres del terminal per a l'entrada estàndard (STDIN)
   TCSANOW indica a tcsetattr que canvii els paràmetres immediatament. */
   tcsetattr( STDIN_FILENO, TCSANOW, &newt);

   /*Llegir un caràcter*/
   charac = (char) getchar();                 
    
   /*restaurar els paràmetres originals*/
   tcsetattr( STDIN_FILENO, TCSANOW, &oldt);
   
}


/**
 * Mostrar a la pantalla el menú del joc i demana una opció.
 * Només accepta una de les opcions correctes del menú ('0'-'8')
 * 
 * Variables globals utilitzades:   
 * rowScreen: Fila de la pantalla on posicionem el cursor.
 * colScreen: Columna de la pantalla on posicionem el cursor.
 * charac   : Caràcter que llegim de teclat.
 * developer:((char *)&developer): Variable definida en el codi assemblador.
 * 
 * Aquesta funció no es crida des d'assemblador
 * i no hi ha definida una subrutina d'assemblador equivalent.
 */
void printMenuP1_C(){
   clearScreen_C();
   rowScreen = 1;
   colScreen = 1;
   gotoxyP1_C();
   printf("                                 \n");
   printf("       P1 Developed by:          \n");
 //  printf("       ( %s )   \n",(char *)&developer);
   printf(" _______________________________ \n");
   printf("|                               |\n");
   printf("|           MAIN MENU           |\n");
   printf("|_______________________________|\n");
   printf("|                               |\n");
   printf("|        1. PosCurScreen        |\n");
   printf("|        2. ShowDigits          |\n");
   printf("|        3. UpdateBoard         |\n");
   printf("|        4. moveCursor          |\n");
   printf("|        5. CalcIndexMat        |\n");
   printf("|        6. OpenCard            |\n");
   printf("|        7. Play Game           |\n");
   printf("|        8. Play Game C         |\n");
   printf("|        0. Exit                |\n");
   printf("|_______________________________|\n");
   printf("|                               |\n");
   printf("|           OPTION:             |\n");
   printf("|_______________________________|\n"); 

   charac=' ';
   while (charac < '0' || charac > '8') {
      rowScreen = 19;
      colScreen = 21;
      gotoxyP1_C();           //posicionar el cursor
      getchP1_C();            //Llegir una opció
      printchP1_C();          //Mostrar opció
   }
   
}


/**
 * Mostrar el tauler de joc a la pantalla. Les línies del tauler.
 * 
 * Variables globals utilitzades:	
 * rowScreen: Fila de la pantalla on posicionem el cursor.
 * colScreen: Columna de la pantalla on posicionem el cursor.
 *  
 * Aquesta funció es crida des de C i des d'assemblador,
 * i no hi ha definida una subrutina d'assemblador equivalent.
 */
void printBoardP1_C(){

   rowScreen = 0;
   colScreen = 0;                                      
   gotoxyP1_C();                                        //Files
                                                        //Tauler
   printf(" _____________________________________ \n"); //01
   printf("|                                     |\n"); //02
   printf("|         M  E  M  O  R  Y   v_1.0    |\n"); //03
   printf("|                                     |\n"); //04
   printf("|  Choose 2 cards and turn them over. |\n"); //05
   printf("|    Try to match all the pairs!      |\n"); //06
   printf("|                                     |\n"); //07
 //Columnes Tauler   12  16  20  24   28         
   printf("|          0   1   2   3   4          |\n"); //08
   printf("|        +---+---+---+---+---+        |\n"); //09
   printf("|      0 |   |   |   |   |   |        |\n"); //10
   printf("|        +---+---+---+---+---+        |\n"); //11
   printf("|      1 |   |   |   |   |   |        |\n"); //12
   printf("|        +---+---+---+---+---+        |\n"); //13
   printf("|      2 |   |   |   |   |   |        |\n"); //14
   printf("|        +---+---+---+---+---+        |\n"); //15
   printf("|      3 |   |   |   |   |   |        |\n"); //16
   printf("|        +---+---+---+---+---+        |\n"); //17
  //Columnes dígits      15       24                 
   printf("|           +----+   +----+           |\n"); //18
   printf("|     Moves |    |   |    | Pairs     |\n"); //19
   printf("|           +----+   +----+           |\n"); //20 
   printf("| (ESC) Exit        Turn Over (Space) |\n"); //21
   printf("| (i)Up    (j)Left  (k)Down  (l)Right |\n"); //22
   printf("|                                     |\n"); //23
   printf("| [                                 ] |\n"); //24
   printf("|_____________________________________|\n"); //25
                          
}


/**
 * Posicionar el cursor a la pantalla, dins el tauler, en funció de la
 * posició del cursor dins la matriu, indicada pel vector (rowcol) 
 * rowcol[0]:fila (0-3) i rowcol[1]:columna (0-4), de tipus short(WORD)2 bytes
 * a partir de la posició [10,12] de la pantalla.
 * Per a calcular la posició del cursor a pantalla (rowScreen) i 
 * (colScreen) utilitzar aquestes fórmules:
 * rowScreen=10+(rc[0]*2)
 * colScreen=12+(rc[1]*4)
 * Per a posicionar el cursor a la pantalla s'ha de cridar a la funció gotoxyP1_C.
 * 
 * Variables globals utilitzades:   
 * rowcol     : Vector on tenim la posició del cursor a la pantalla.
 * rowScreen  : Fila on volem posicionar el cursor a la pantalla.
 * colScreen  : Columna on volem posicionar el cursor a la pantalla.
 * 
 * Aquesta funció no es crida des d'assemblador.
 * A la subrutina d'assemblador equivalent 'posCurScreenP1',  
 * el pas de paràmetres és equivalent.
 */
void posCurScreenP1_C() {

   rowScreen=10+(rowcol[0]*2);
   colScreen=12+(rowcol[1]*4);
   gotoxyP1_C();
   
}


/**
 * Converteix un valor (value) de tipus int(4 bytes) (entre 0 i 99) en  
 * dos caràcters ASCII que representin aquest valor. (27 -> '2' '7').
 * S'ha de dividir el valor entre 10, el quocient representarà les 
 * desenes i el residu les unitats, i després s'han de convertir a ASCII
 * sumant '0' o 48(codi ASCII de '0') a les unitats i a les desenes.
 * Mostra els dígits (caràcter ASCII) a partir de la fila indicada
 * per la variable (rowScreen) i a la columna indicada per la variable
 * (colScreen).
 * Per a posicionar el cursor es cridar a la funció gotoxyP1_C i per a 
 * mostrar els caràcters a la funció printchP1_C.
 * 
 * Variables globals utilitzades:	
 * rowScreen: Fila de la pantalla on posicionem el cursor.
 * colScreen: Columna de la pantalla on posicionem el cursor.
 * charac   : Caràcter que llegim de teclat.
 * value    : Valor que volem mostrar.
 * 
 * Aquesta funció no es crida des d'assemblador.
 * Hi ha un subrutina en assemblador equivalent 'showDigitsP1',  
 */
 void showDigitsP1_C() {
	
	char d, u;
	d = value / 10;      //Decenes
	d = d + '0';
	charac = d;
    gotoxyP1_C();   
	printchP1_C();
	
	u = value % 10;      //Unitats
	u = u + '0';
	charac = u;
    colScreen++;
	gotoxyP1_C();   
	printchP1_C();
	
}


/**
 * Mostrar els valors de la matriu (mOpenCards) dins el tauler, 
 * a les posicions corresponents, els moviments (moves) i les parelles 
 * fetes (pairs). 
 * S'ha de recórrer tota la matriu (mOpenCards), cada posició és de 
 * tipus char(BYTE)1byte, i per a cada element de la matriu fer:
 * Posicionar el cursor en el tauler en funció de les variables 
 * (rowScreen) fila i (colScreen) columna cridant la funció gotoxyP1_C.
 * Les variables (rowScreen) i (colScreen) s'inicialitzaran a 10 i 14
 * respectivament, que és la posició a pantalla de la casella [0][0].
 * Mostrar els caràcters de cada posició de la matriu (mOpenCards) 
 * cridant la funció printchP1_C.
 * Després, mostrar els moviments (moves) de tipus int(DWORD)4bytes, 
 * a partir de la posició [19,15] de la pantalla i mostrar les parelles
 * fetes (pairs) de tipus int(DWORD)4bytes, a partir de la 
 * posició [19,24] de la pantalla cridant la funció showDigitsP1_C.
 * 
 * Variables globals utilitzades:		
 * rowScreen  : Fila on volem posicionar el cursor a la pantalla.
 * colScreen  : Columna on volem posicionar el cursor a la pantalla.
 * mOpenCards : Matriu on guardem les targetes del joc.
 * charac     : Caràcter que volem mostrar.
 * value      : Valor que volem mostrar.
 * moves      : Parelles que s'han intentat fer amb èxit o sense.
 * pairs      : Parelles que s'han fet.
 * 
 * 
 * Aquesta funció no es crida des d'assemblador.
 * Hi ha una subrutina en assemblador equivalent 'updateBoardP1'.
 */
void updateBoardP1_C(){
   
   int i,j;
   char c;
   
   rowScreen=10;
   for (i=0;i<ROWDIM;i++){
	  colScreen=12;
      for (j=0;j<COLDIM;j++){
         gotoxyP1_C();
         charac = mOpenCards[i][j];
         printchP1_C();
         colScreen = colScreen + 4;
      }
      rowScreen = rowScreen + 2;
   }
   
   rowScreen = 19;
   colScreen = 15;
   value = moves;
   showDigitsP1_C();
   colScreen = 24;
   value = pairs;
   showDigitsP1_C();
   
}


/**
 * Actualitzar la posició del cursor dins la matriu actualitzant el 
 * vector (rowcol):vector de tipus short(WORD)2bytes, amb la fila 
 * rowcol[0] i la columna rowcol[1] de la posició del cursor dins 
 * la matriu, en funció de la tecla premuda que tenim a la 
 * variable (charac) de tipus char(BYTE)1byte,
 * (i: amunt, j:esquerra, k:avall, l:dreta).
 * Comprovar que no sortim de la matriu, el vector (rowcol) només
 * pot prendre els valors de les posicions dins de la matriu.
 * Les files i les columnes s'incrementen de 1 en 1 perquè cada
 * posició de la matriu és de tipus char(BYTE)1byte.
 * Si s'ha de sortir de la matriu, no fer el moviment.
 * NO s'ha de posicionar el cursor a la pantalla cridant posCurScreenP1_C.
 * 
 * Variables globals utilitzades:	
 * charac : Caràcter que llegim de teclat.
 * rowcol : Vector per a indicar la posició del cursor a la matriu.
 * 
 * Aquesta funció no es crida des d'assemblador.
 * Hi ha una subrutina en assemblador equivalent 'moveCursorP1'.
 */
void moveCursorP1_C(){
 
   switch(charac){
      case 'i': //amunt
         if (rowcol[0]>0) rowcol[0]=rowcol[0]-1;
      break;
      case 'k': //avall
         if (rowcol[0]<(ROWDIM-1)) rowcol[0]=rowcol[0]+1;
      break;
      case 'j': //esquerra
         if (rowcol[1]>0) rowcol[1]=rowcol[1]-1;
      break;
      case 'l': //dreta
         if (rowcol[1]<(COLDIM-1)) rowcol[1]=rowcol[1]+1;
      break;
      
   }

}


/**
 * Obrir la targeta de la matriu (mCards) de la posició indicada pel
 * cursor dins la matriu i que tenim al vector (rowcol). 
 * rowcol[0] fila i rowcol[1] columna.
 * Si la targeta no està girada (!='x') posar-la a la matriu (mOpenCards)
 * per a que es mostri.
 * Marcar-la amb una 'x'(minúscula) a la mateixa posició de la matriu 
 * (mCards) per a saber que està girada.
 * Passar al següent estat (state++)
 * 
 * NO s'ha de mostrar la matriu amb els canvis, es fa a updateBoardP1_C().
 * 
 * Variables globals utilitzades:
 * rowcol     : Vector on tenim la posició del cursor dins la matriu.
 * mCards     : Matriu on guardem les targetes del joc.
 * mOpenCards : Matriu on tenim les targetes obertes del joc.
 * state      : Estat del joc.
 * 
 * Aquesta funció no es crida des d'assemblador.
 * Hi ha una subrutina en assemblador equivalent 'openCardP1'.
 */
void openCardP1_C(){

   char aux;
   int i = rowcol[0];
   int j = rowcol[1];
   
   if (mCards[i][j] != 'x') {
      mOpenCards[i][j] = mCards[i][j];
      mCards[i][j] = 'x';
      state++;
   }
   
}


/**
 * Mostra un missatge a sota del tauler segons el valor de la variable
 * state)  0: 0 targetes obertes.
 *         1: 1 Targeta oberta.
 *         2: 2 Targetes obertes.
 *         5: Sortir, hem premut la tecla 'ESC' per a sortir.
 * Si (state>1) demanar que es premi una tecla per a poder-lo llegir.
 *         
 * Variables globals utilitzades:	
 * state    : Estat del joc. (0: Sortir, hem premut la tecla 'ESC').
 * rowScreen: Fila de la pantalla on posicionem el cursor.
 * colScreen: Columna de la pantalla on posicionem el cursor. 
 *  
 * S'ha definit un subrutina en assemblador equivalent 'printMessageP1' per
 * a cridar aquesta funció guardant l'estat dels registres del processador.
 * Això es fa perquè les funcions de C no mantenen l'estat dels 
 * registres.
 */
void printMessageP1_C() {

   rowScreen = 24;
   colScreen = 4;
   gotoxyP1_C();
   switch(state){
	  case 0:
         printf("...  Turn Over FIRST card !!! ...");
      break;
      case 1:
         printf("...  Turn Over SECOND card!!! ...");
      break; 
      case 5:
         printf("<<<<<< EXIT: (ESC) Pressed >>>>>>");
      break;
   }
   if (state > 1) getchP1_C();     
}


/**
 * Joc del Memory
 * Funció principal del joc.
 * Trobat totes les parelles del tauler (10 parelles), girant les
 * targetes de dues en dues. Com a màxim es poden fer 15 intents.
 * 
 * Pseudo-codi:
 * Inicialitzar l'estat del joc, (state=0).
 * Esborrar la pantalla  (cridar la funció clearScreen_C).
 * Mostrar el tauler de joc (cridar la funció printBoardP1_C).
 * Actualitzar el tauler de joc i els valors dels moviments fets (moves)
 * i de les parelles fetes (pairs) cridant la funció updateBoardP1_C.
 * Mentre (state<3) fer:
 *   Mostrar un missatge,  segons el valor de la variable (state),
 *   per a indicar que s'ha de fer, cridant la funció printMessageP1_C.
 *   Actualitzar la posició del cursor a la pantalla a partir del vector
 *   ([rowcol]) (fila (rowcol[0]) i la columna (rowcol[1])) amb la posició
 *   del cursor dins la matriu, cridant la funció posCurScreenP1_C.
 *   Llegir una tecla, cridar la funció getchP1_C. 
 *   Segons la tecla llegida cridarem a les subrutines que corresponguin.
 *     - ['i','j','k' o 'l'] desplaçar el cursor segons la direcció 
 *       triada, cridant la funció moveCursorP1_C).
 *     - '<SPACE>'(codi ASCII 32) girar la targeta on hi ha el cursor
 *       cridant la funció openCardP1_C.
 *       Actualitzar el tauler de joc i els valors dels moviments fets (moves)
 *       i de les parelles fetes (pairs) cridant la funció updateBoardP1_C.
 *       Si s'han girat dues targetes (state>1) incrementar els moviments (moves)
 *       i posar (state=0) per a tornar a fer una nova parella.
 *    - '<ESC>'  (codi ASCII 27) posar (state = 5) per a sortir.
 *       No sortira si només s'ha girat una targeta (state!=1).
 * Fi mentre.
 * Sortir: S'acaba el joc.
 * 
 * Variables globals utilitzades:	
 * rowcol     : Vector on tenim la posició del cursor dins la matriu.
 * state      : Indica l'estat del joc. 0:sortir, 1:jugar.
 * charac     : Caràcter que llegim de teclat.
 * 
 * Aquesta funció no es crida des d'assemblador.
 * S'ha definit una subrutina d'assemblador equivalent 'playP1' per a cridar
 * les subrutines del joc definides en assemblador.
 */
void playP1_C(){

   state = 0;            //Estat per a començar a jugar   
   rowcol[0] = 2;        //Posició inicial del cursor dins la matriu.
   rowcol[1] = 1;
   
   clearScreen_C();
   printBoardP1_C();
   updateBoardP1_C();
   
   while (state < 3) {   //Bucle principal.
	  printMessageP1_C();
      posCurScreenP1_C();//Posicionar Curso al tauler 
      getchP1_C();       //llegir una tecla.
   
      if (charac>='i' && charac<='l') {
         moveCursorP1_C();
      }
      if (charac==32) {
         openCardP1_C();
         
         if (state > 1) {
			 moves++;
			 state = 0;
		 }
		 updateBoardP1_C();
		 
      }  
      if ((charac==27) && (state!=1)) {
         state = 5;
      }
   }
   
}


/**
 * Programa Principal
 * 
 * ATENCIÓ: Podeu provar la funcionalita de les subrutines que s'han de
 * desenvolupar treient els comentaris de la crida a la funció 
 * equivalent implementada en C que hi ha sota a cada opció.
 * Per al joc complet hi ha una opció per la versió en assemblador i 
 * una opció pel joc en C.
 */
int main(void){
   
   int op=1;
   unsigned char state=1;     
   //rowcol[0]=3;
   //rowcol[1]=4;
   
   while (charac!='0') {
      clearScreen_C();
      printMenuP1_C();    //Mostrar menú i demana opció
      op = charac;
      switch(op){
          case '1': //Posicionar el cursor a la pantalla, dins el tauler.
            printf(" %c",op);
            clearScreen_C();    
            printBoardP1_C();   
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press any key ");
            rowcol[0]=3;
            rowcol[1]=4;
            //=======================================================        
            posCurScreenP1();
            //posCurScreenP1_C();
            //=======================================================
            getchP1_C();
         break; //Converteix un valor (entre 0 i 99) en 2 dos caràcters ASCII.
         case '2': //Mostra 
            printf(" %c",op);
            clearScreen_C();    
            printBoardP1_C();
            rowScreen = 19;
            colScreen = 15;
            value = 99;
            //=======================================================        
            showDigitsP1();
            //showDigitsP1_C();
            //=======================================================
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press any key ");
            getchP1_C();
         break;
         case '3': //Actualitzar el contingut del tauler.
            clearScreen_C();       
            printBoardP1_C();      
            //=======================================================
            updateBoardP1();
            //updateBoardP1_C();
            //=======================================================
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press any key ");
            getchP1_C();
         break;
         case '4': //Actualitzar posició del cursor al tauler. 
            clearScreen_C();
            printBoardP1_C();
            updateBoardP1_C();
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press i,j,k,l ");
            rowcol[0]=3;
            rowcol[1]=0;
            posCurScreenP1_C();
            getchP1_C();   
            if (charac>='i' && charac<='l') {
			//=======================================================
            moveCursorP1();
            //moveCursorP1_C();  
            //=======================================================
            }
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press any key ");
            posCurScreenP1_C();
            getchP1_C();
         break;
         case '5': //Calcular l'índex per a accedir a la matriu 4x5.
            clearScreen_C();
            printBoardP1_C();
            row=3;
            col=4;
            rowcol[0]=3;
            rowcol[1]=4;
            //===Cridem subrutina calIndexP1 en assemblador===========
              calcIndexP1();
              charac = mCards[indexMat/5][indexMat%5];             
            //=======================================================
            openCardP1_C();
            updateBoardP1_C();
            rowScreen = 26;
            colScreen = 1;
            gotoxyP1_C();
            printf("Index: C: mCards[%i][%i] - Assembler: [ mCards + %i ] = %c  >Press any key<",indexMat/5, indexMat%5, indexMat, charac);
            getchP1_C();
       break;
       case '6': //Obrir una targeta on hi ha el cursor
            clearScreen_C();
            printBoardP1_C();
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf("Press <space> ");
            updateBoardP1_C();
            rowcol[0]=1;
            rowcol[1]=2;
            posCurScreenP1_C();
            state = 0;
            getchP1_C(); 
            if (charac>=' ') {
			//=======================================================
               openCardP1();
               //openCardP1_C();
            //=======================================================
            }
            updateBoardP1_C();
            printMessageP1_C();
            rowScreen = 26;
            colScreen = 12;
            gotoxyP1_C();
            printf(" Press any key ");
            getchP1_C();
         break;
         case '7': //Joc complet en Assemblador  
            //=======================================================
            playP1();
            //=======================================================
         break;
         case '8': //Joc complet en C
            //=======================================================
            playP1_C();
            //=======================================================
         break;
     }
   }
   printf("\n\n");
   
   return 0;
   
}
