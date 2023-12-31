       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP002.
       AUTHOR. RONAN FELIPE JORGE
      ***********************************************
      * CADASTRO DE CLIENTE   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CLI-CPF 
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CLI-NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
                03 CLI-CPF      PIC 9(8).
                03 CLI-NOME     	PIC X(30).
				03 CLI-FANTASIA     	PIC X(12).
                03 CLI-DDD				PIC 9(2).
                03 CLI-NUMTEL      PIC 9(9).
                03 CLI-EMAIL      PIC X(30).
                03 CLI-CEP      PIC X(8).
                03 CLI-CONTATO      PIC X(30).
                03 CLI-NUM-LOGR      PIC 9(5).
                03 CLI-COMPL-LOGR      PIC X(12).      				
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 W-CONT       	PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    	PIC X(01) VALUE SPACES.
       77 ST-ERRO   	PIC X(02) VALUE "00". 
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS         	PIC X(50) VALUE SPACES.
       77 LIMPA      	PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA01.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "   ==========================  CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  "DE CLIENTE  =========================  ".
           05  LINE 07  COLUMN 01 
               VALUE  "                  CPF :".
           05  LINE 08  COLUMN 01 
               VALUE  " NOME OU RAZAO SOCIAL :".
		   05  LINE 09  COLUMN 01 
               VALUE  "        NOME FANTASIA :".
           05  LINE 10  COLUMN 01 
               VALUE  "             TELEFONE : (".
           05  LINE 10  COLUMN 28 
               VALUE  ")".  
           05  LINE 11  COLUMN 01 
               VALUE  "                EMAIL :".
           05  LINE 12  COLUMN 01 
               VALUE  "          CEP CLIENTE :".
           05  LINE 13  COLUMN 01 
               VALUE  "              CONTATO :".
           05  LINE 14  COLUMN 01 
               VALUE  " NUMERO DO LOGRADOURO :".
           05  LINE 15  COLUMN 01 
               VALUE  "          COMPLEMENTO :".			   
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCLI-CPF
               LINE 07  COLUMN 25  PIC 99999.999
               USING  CLI-CPF.
           05  TCLI-NOME
               LINE 08  COLUMN 25  PIC X(35)
               USING  CLI-NOME.
		   05  TCLI-FANTASIA
               LINE 09  COLUMN 25  PIC X(12)
               USING  CLI-FANTASIA.
           05  TCLI-DDD
               LINE 10  COLUMN 26  PIC 9(2)
               USING  CLI-DDD.
           05  TCLI-NUMTEL
               LINE 10  COLUMN 30  PIC 9(9)
               USING  CLI-NUMTEL.
           05  TCLI-EMAIL
               LINE 11  COLUMN 25  PIC X(35)
               USING  CLI-EMAIL.
           05  TCLI-CEP
               LINE 12  COLUMN 25  PIC X(08)
               USING  CLI-CEP.
           05  TCLI-CONTATO
               LINE 13  COLUMN 25  PIC X(30)
               USING  CLI-CONTATO.
           05  TCLI-NUM-LOGR
               LINE 14  COLUMN 25  PIC 9(5)
               USING  CLI-NUM-LOGR.
           05  TCLI-COMPL-LOGR
               LINE 15  COLUMN 25  PIC X(12)
               USING  CLI-COMPL-LOGR.			  
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQCLI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQCLI
                      CLOSE ARQCLI
                      MOVE "* ARQ CLIENTE SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQ CLIENTE" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       R01.
                MOVE SPACES TO CLI-NOME CLI-FANTASIA CLI-EMAIL.
				MOVE SPACES TO CLI-CEP CLI-CONTATO.
                MOVE SPACES TO CLI-COMPL-LOGR.
                MOVE ZEROS  TO CLI-CPF CLI-DDD CLI-NUMTEL CLI-NUM-LOGR.
                DISPLAY TELA01.
       R02.
                ACCEPT TCLI-CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQCLI
                   GO TO ROT-FIM.
                IF CLI-CPF = ZEROS 
                   MOVE "*** CPF NAO PODE SER ZEROS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R02.
       LER-CPF01.
                MOVE 0 TO W-SEL
                READ ARQCLI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELA01
                      MOVE "*** CPF JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. CLIENTE"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       R03.
                ACCEPT TCLI-NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R02.
                IF CLI-NOME = SPACES 
                   MOVE "NOME DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R03.
       R04.
                ACCEPT TCLI-FANTASIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R03.
                IF CLI-FANTASIA = SPACES 
                   MOVE "FANT DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R04.
       R05.
                ACCEPT TCLI-DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R04.
                IF CLI-DDD = ZEROS 
                   MOVE "DDD DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R05.
       R06.
	            ACCEPT TCLI-NUMTEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R05.
                IF CLI-NUMTEL = ZEROS 
                   MOVE "TELEFONE DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R06.
       R07.
                ACCEPT TCLI-EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R06.
                IF CLI-EMAIL = SPACES 
                   MOVE "EMAIL DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R07.
       R08.
                ACCEPT TCLI-CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R07.
                IF CLI-CEP = SPACES 
                   MOVE "CEP DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R08.
       R09.
	            ACCEPT TCLI-CONTATO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R08.
                IF CLI-CONTATO = SPACES 
                   MOVE "CONTATO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R09.
       R10.
                ACCEPT TCLI-NUM-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R09.
                IF CLI-NUM-LOGR = ZEROS 
                   MOVE "NUMERO DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R10.
       R11.
                ACCEPT TCLI-COMPL-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R10.
                IF CLI-COMPL-LOGR = SPACES 
                   MOVE "COMPL. DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R11.
				IF W-SEL = 1
							  GO TO ALT-OPC. 							  
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R11.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR01.
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01.
                IF ST-ERRO = "22"
                      MOVE "*** CLIENTE JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CLIENTE "
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO R01.
                IF W-ACT = 03
                   GO TO R03.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R11.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CLIENTE"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
