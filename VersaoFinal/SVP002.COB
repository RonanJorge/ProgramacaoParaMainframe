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
				03 CLI-FANTASIA     	PIC X(30).
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
           05  LINE 08  COLUMN 01 
               VALUE  "                  CPF :".
           05  LINE 10  COLUMN 01 
               VALUE  " NOME OU RAZAO SOCIAL :".
		   05  LINE 12  COLUMN 01 
               VALUE  "        NOME FANTASIA :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCLI-CPF
               LINE 08  COLUMN 25  PIC 99999.999
               USING  CLI-CPF.
           05  TCLI-NOME
               LINE 10  COLUMN 25  PIC X(35)
               USING  CLI-NOME.
		   05  TCLI-FANTASIA
               LINE 12  COLUMN 25  PIC X(35)
               USING  CLI-FANTASIA.
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
                      MOVE "* ARQUIVO CEP SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       R1.
                MOVE SPACES TO CLI-NOME CLI-FANTASIA. 
                MOVE ZEROS  TO CLI-CPF.
                DISPLAY TELA01.
       R2.
                ACCEPT TCLI-CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQCLI
                   GO TO ROT-FIM.
                IF CLI-CPF = ZEROS 
                   MOVE "*** CPF NAO PODE SER ZEROS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2.
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
                      MOVE "ERRO NA LEITURA ARQ. CPF"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       R3.
                ACCEPT TCLI-NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R2.
                IF CLI-NOME = SPACES 
                   MOVE "NOME DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.
	   R4.
                ACCEPT TCLI-FANTASIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R3.
                IF CLI-FANTASIA = SPACES 
                   MOVE "FANTASIA DEVE SER DIF. DE BRANCOS " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.
				IF W-SEL = 1
							  GO TO ALT-OPC. 
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                      MOVE "*** CPF JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CPF "
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
                   GO TO R1.
                IF W-ACT = 03
                   GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO NOTAS"   TO MENS
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
