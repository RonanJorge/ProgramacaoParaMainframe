       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP003.
       AUTHOR. RONAN FELIPE JORGE
      ***********************************************
      * CADASTRO DE FORNECEDOR   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS FOR-CPF 
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS FOR-NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
                03 FOR-CPF      PIC 9(8).
                03 FOR-NOME     	PIC X(30).
				03 FOR-FANTASIA     	PIC X(12).
                03 FOR-DDD				PIC 9(2).
                03 FOR-NUMTEL      PIC 9(9).
                03 FOR-EMAIL      PIC X(30).
                03 FOR-CEP      PIC X(8).
                03 FOR-CONTATO      PIC X(30).
                03 FOR-NUM-LOGR      PIC 9(5).
                03 FOR-COMP-LOGR      PIC X(12).      				
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
       01  TELAFOR.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "   ==========================  CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  "DE FORNECEDOR  ======================  ".
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
               VALUE  "           CEP FORNEC :".
           05  LINE 13  COLUMN 01 
               VALUE  "              CONTATO :".
           05  LINE 14  COLUMN 01 
               VALUE  " NUMERO DO LOGRADOURO :".
           05  LINE 15  COLUMN 01 
               VALUE  "          COMPLEMENTO :".			   
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TFOR-CPF
               LINE 07  COLUMN 25  PIC 99999.999
               USING  FOR-CPF.
           05  TFOR-NOME
               LINE 08  COLUMN 25  PIC X(35)
               USING  FOR-NOME.
		   05  TFOR-FANTASIA
               LINE 09  COLUMN 25  PIC X(12)
               USING  FOR-FANTASIA.
           05  TFOR-DDD
               LINE 10  COLUMN 26  PIC 9(2)
               USING  FOR-DDD.
           05  TFOR-NUMTEL
               LINE 10  COLUMN 30  PIC 9(9)
               USING  FOR-NUMTEL.
           05  TFOR-EMAIL
               LINE 11  COLUMN 25  PIC X(35)
               USING  FOR-EMAIL.
           05  TFOR-CEP
               LINE 12  COLUMN 25  PIC X(08)
               USING  FOR-CEP.
           05  TFOR-CONTATO
               LINE 13  COLUMN 25  PIC X(30)
               USING  FOR-CONTATO.
           05  TFOR-NUM-LOGR
               LINE 14  COLUMN 25  PIC 9(5)
               USING  FOR-NUM-LOGR.
           05  TFOR-COMP-LOGR
               LINE 15  COLUMN 25  PIC X(12)
               USING  FOR-COMP-LOGR.			  
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQFOR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQFOR
                      CLOSE ARQFOR
                      MOVE "* ARQ FORNEC SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQ FORNEC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       R01.
                MOVE SPACES TO FOR-NOME FOR-FANTASIA FOR-EMAIL.
				MOVE SPACES TO FOR-CEP FOR-CONTATO.
                MOVE SPACES TO FOR-COMP-LOGR.
                MOVE ZEROS  TO FOR-CPF FOR-DDD FOR-NUMTEL FOR-NUM-LOGR.
                DISPLAY TELAFOR.
       R02.
                ACCEPT TFOR-CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQFOR
                   GO TO ROT-FIM.
                IF FOR-CPF = ZEROS 
                   MOVE "*** CPF NAO PODE SER ZEROS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R02.
       LER-CPF01.
                MOVE 0 TO W-SEL
                READ ARQFOR
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELAFOR
                      MOVE "*** CPF JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. FORNEC "   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       R03.
                ACCEPT TFOR-NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R02.
                IF FOR-NOME = SPACES 
                   MOVE "NOME DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R03.
       R04.
                ACCEPT TFOR-FANTASIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R03.
                IF FOR-FANTASIA = SPACES 
                   MOVE "FANT DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R04.
       R05.
                ACCEPT TFOR-DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R04.
                IF FOR-DDD = ZEROS 
                   MOVE "DDD DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R05.
       R06.
	            ACCEPT TFOR-NUMTEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R05.
                IF FOR-NUMTEL = ZEROS 
                   MOVE "TELEFONE DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R06.
       R07.
                ACCEPT TFOR-EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R06.
                IF FOR-EMAIL = SPACES 
                   MOVE "EMAIL DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R07.
       R08.
                ACCEPT TFOR-CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R07.
                IF FOR-CEP = SPACES 
                   MOVE "CEP DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R08.
       R09.
	            ACCEPT TFOR-CONTATO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R08.
                IF FOR-CONTATO = SPACES 
                   MOVE "CONTATO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R09.
       R10.
                ACCEPT TFOR-NUM-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R09.
                IF FOR-NUM-LOGR = ZEROS 
                   MOVE "NUMERO DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R10.
       R11.
                ACCEPT TFOR-COMP-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R10.
                IF FOR-COMP-LOGR = SPACES 
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
                WRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01.
                IF ST-ERRO = "22"
                      MOVE "*** FORNECEDOR JA EXISTE ***     " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE FORNEC "
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
                DELETE ARQFOR RECORD
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
                REWRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                MOVE "ERRO NA ALTERACAO DO REGISTRO FORNEC."   TO MENS
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
