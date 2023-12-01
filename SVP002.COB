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
                03 CLI-TP      		PIC X(01).
                03 CLI-CPF         	PIC 9(14).
                03 CLI-NOME        	PIC X(30).
                03 CLI-FANTASIA     PIC X(12).
                03 CLI-DDD          PIC 9(02).
				03 CLI-NUMTEL  		PIC 9(09).
                03 CLI-EMAIL       	PIC X(30).
                03 CLI-CEP        	PIC X(08).
				03 CLI-CONTATO     	PIC X(30).
                03 CLI-NUM-LOGR		PIC 9(05).
                03 CLI-COMPL-LOGR   PIC X(12).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        	PIC 9(01) VALUE ZEROS.
       77 W-CONT       	PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    	PIC X(01) VALUE SPACES.
       77 ST-ERRO     	PIC X(02) VALUE "00". 
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
      *
       01 TXUF.
            03 UF-SIGLA     PIC X(02) VALUE SPACES.
            03 UF-TEXTO   PIC X(20) VALUE SPACES.
      *
       01 TBUF.
             03 FILLER     PIC X(22) VALUE "ACACRE".
             03 FILLER     PIC X(22) VALUE "ALALAGOAS".
             03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
             03 FILLER     PIC X(22) VALUE "APAMAPA".
             03 FILLER     PIC X(22) VALUE "BABAHIA".
             03 FILLER     PIC X(22) VALUE "CECEARA".
             03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
             03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
             03 FILLER     PIC X(22) VALUE "GOGOIAS".
             03 FILLER     PIC X(22) VALUE "MAMARANHÃO".
             03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
             03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
             03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
             03 FILLER     PIC X(22) VALUE "PAPARA".
             03 FILLER     PIC X(22) VALUE "PBPARAIBA".
             03 FILLER     PIC X(22) VALUE "PRPARANÁ".
             03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
             03 FILLER     PIC X(22) VALUE "PIPIAUI".
             03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
             03 FILLER     PIC X(22) VALUE "RNR. GRANDE DO NORTE".
             03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
             03 FILLER     PIC X(22) VALUE "RORONDONIA".
             03 FILLER     PIC X(22) VALUE "RRRORAIMA".
             03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
             03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
             03 FILLER     PIC X(22) VALUE "SESERGIPE".
             03 FILLER     PIC X(22) VALUE "TOTOCANTINS".
      *		   
       01 TBUFR REDEFINES TBUF.
             03 TABUF    PIC X(22) OCCURS 27 TIMES.
       01 IND            PIC 9(02) VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA01.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "   =========================== CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  "DE CLIENTE =========================   ".
           05  LINE 06  COLUMN 01 
               VALUE  "       TIPO DE PESSOA :".
           05  LINE 07  COLUMN 01 
               VALUE  "           CPF / CNPJ :".
		   05  LINE 08  COLUMN 01 
               VALUE  " NOME OU RAZÃO SOCIAL :".
           05  LINE 09  COLUMN 01 
               VALUE  "        NOME FANTASIA :".
           05  LINE 10  COLUMN 01 
               VALUE  "      DDD DO TELEFONE :".
           05  LINE 11  COLUMN 01 
               VALUE  "   NUMERO DO TELEFONE :".
           05  LINE 12  COLUMN 01 
               VALUE  "                EMAIL :".
           05  LINE 13  COLUMN 01 
               VALUE  "          CEP CLIENTE :".
           05  LINE 14  COLUMN 01 
               VALUE  "              CONTATO :".
           05  LINE 15  COLUMN 01 
               VALUE  "   NÚMERO DO ENDEREÇO :".
           05  LINE 16  COLUMN 01 
               VALUE  "          COMPLEMENTO :".		   
		   05  LINE 23  COLUMN 01  
               VALUE  "             MENSAGEM :".
			   
		   05  TCLI-TP
               LINE 06  COLUMN 25  PIC X(01)
               USING  CLI-TP.
           05  TCLI-CPF
               LINE 07  COLUMN 25  PIC 99999999999999
               USING  CLI-CPF.
           05  TCLI-NOME
               LINE 08  COLUMN 25  PIC X(30)
               USING  CLI-NOME.
           05  TCLI-FANTASIA
               LINE 09  COLUMN 25  PIC X(12)
               USING  CLI-FANTASIA.
           05  TCLI-DDD
               LINE 10  COLUMN 25  PIC 9(02)
               USING  CLI-DDD.
           05  TCLI-NUMTEL
               LINE 11  COLUMN 25  PIC 9(09)
               USING  CLI-NUMTEL.
		   05  TCLI-EMAIL
               LINE 12  COLUMN 25  PIC X(30)
               USING  CLI-EMAIL.
           05  TCLI-CEP
               LINE 13  COLUMN 25  PIC X(08)
               USING  CLI-CEP.
           05  TCLI-CONTATO
               LINE 14  COLUMN 25  PIC X(30)
               USING  CLI-CONTATO.
           05  TCLI-NUM-LOGR
               LINE 15  COLUMN 25  PIC 9(05)
               USING  CLI-NUM-LOGR.
           05  TCLI-COMPL-LOGR
               LINE 16  COLUMN 25  PIC X(12)
               USING  CLI-COMPL-LOGR.
			   
			   
	   01  FTELA2.
           05  LINE 06  COLUMN 31 
               VALUE  "  ESTADOS   ".
           05  LINE 08  COLUMN 31 
               VALUE  "  AC- ACRE    ".
           05  LINE 09  COLUMN 31 
               VALUE  "  AL- ALAGOAS ".
           05  LINE 10  COLUMN 31 
               VALUE  "  AM- AMAZONAS".
           05  LINE 11  COLUMN 31 
               VALUE  "  AP- AMAPA".
           05  LINE 12  COLUMN 31 
               VALUE  "  BA- BAHIA".
           05  LINE 13  COLUMN 31 
               VALUE  "  CE- CEARA".
           05  LINE 14  COLUMN 31 
               VALUE  "  DF- DISTRITO FEDERAL".
           05  LINE 15  COLUMN 31 
               VALUE  "  ES- ESPIRITO SANTO".
           05  LINE 16  COLUMN 31 
               VALUE  "  GO- GOIAS           ".
           05  LINE 17  COLUMN 31 
               VALUE  "  MA- MARANHAO    ".
           05  LINE 18  COLUMN 31 
               VALUE  "  MT- MATO GROSSO".
           05  LINE 19  COLUMN 31 
               VALUE  "  MS- MATO GROSSO DO SUL".
           05  LINE 20  COLUMN 31 
               VALUE  "  MG- MINAS GERAIS".
           05  LINE 21  COLUMN 31 
               VALUE  "  PA- PARA".
           05  LINE 08  COLUMN 55 
               VALUE  "  PB- PARAIBA".
           05  LINE 09  COLUMN 55 
               VALUE  "  PR- PARANA".
           05  LINE 10  COLUMN 55 
               VALUE  "  PE- PERNAMBUCO".
           05  LINE 11  COLUMN 55 
               VALUE  "  PI- PIAUI           ".
		   05  LINE 12  COLUMN 55 
               VALUE  "  RJ- RIO DE JANEIRO".
           05  LINE 13  COLUMN 55 
               VALUE  "  RN- R. GRANDE DO NORTE".
           05  LINE 14  COLUMN 55 
               VALUE  "  RS- RIO GRANDE DO SUL".
           05  LINE 15  COLUMN 55 
               VALUE  "  RO- RONDONIA".
           05  LINE 16  COLUMN 55 
               VALUE  "  RR- RORAIMA".
           05  LINE 17  COLUMN 55 
               VALUE  "  SC- SANTA CATARINA".
           05  LINE 18  COLUMN 55 
               VALUE  "  SP- SAO PAULO".
           05  LINE 19  COLUMN 55 
               VALUE  "  SE- SERGIPE".
           05  LINE 20  COLUMN 55 
               VALUE  "  TO- TOCANTINS          ".
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
                      MOVE "* ARQUIVO CLIENTE SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERT. DO ARQUIVO CLIENTE" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE SPACES TO CLI-TP CLI-NOME CLI-FANTASIA CLI-EMAIL.
                MOVE SPACES TO CLI-CEP CLI-CONTATO CLI-COMPL-LOGR. 
                MOVE ZEROS  TO CLI-CPF CLI-DDD CLI-NUMTEL CLI-NUM-LOGR.
                DISPLAY TELA01.
	   INC-002.
                ACCEPT TCLI-TP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQCLI
                   GO TO ROT-FIM.
                IF CLI-TP = SPACES 
                   MOVE "*** TIPO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
	   INC-003.
                ACCEPT TCLI-CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF CLI-CPF = ZEROS 
                   MOVE "*** CPF INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       
       LER-CPF01.
                MOVE 0 TO W-SEL
                READ ARQCLI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
      *                PERFORM INC-006A THRU INC-006B
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
       INC-004.
                ACCEPT TCLI-NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF CLI-NOME = SPACES 
                   MOVE "*** INSIRA O NOME DO CLIENTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
	   INC-005.
                ACCEPT TCLI-FANTASIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF CLI-FANTASIA = SPACES 
                   MOVE "*** INSIRA O NOME FANTASIA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
	   INC-006.
                ACCEPT TCLI-DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF CLI-DDD = ZEROS 
                   MOVE "*** INSIRA O DDD ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
	   INC-007.
                ACCEPT TCLI-NUMTEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF CLI-NUMTEL = ZEROS 
                   MOVE "*** INSIRA O TELEFONE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.
	   INC-008.
                ACCEPT TCLI-EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF CLI-EMAIL = SPACES 
                   MOVE "*** INSIRA UM EMAIL ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-008.
	   INC-009.
                ACCEPT TCLI-CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF CLI-CEP = SPACES 
                   MOVE "*** INSIRA O CEP ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
	   INC-010.
                ACCEPT TCLI-CONTATO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF CLI-CONTATO = SPACES 
                   MOVE "*** INSIRA UM CONTATO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-010.
	   INC-011.
                ACCEPT TCLI-NUM-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
                IF CLI-NUM-LOGR = ZEROS 
                   MOVE "*** INSIRA O NUMERO DO LOGR. ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-011.
	   INC-012.
                ACCEPT TCLI-COMPL-LOGR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
		
	   
      * 
				IF W-SEL = 1
								GO TO ALT-OPC. 
	   INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
				IF W-ACT = 02 GO TO INC-012.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQ. DE NOTAS" TO MENS
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
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-004.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
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
