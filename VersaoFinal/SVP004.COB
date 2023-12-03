       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP004.
       AUTHOR. RONAN FELIPE JORGE
      ***********************************************
      * CADASTRO DE PRODUTOS   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PRO-COD 
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS PRO-DESC
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQPRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".
       01 REGPRO.
                03 PRO-COD          PIC 9(8).
                03 PRO-DESC         PIC X(30).
				03 PRO-UNID         PIC X(02).
                03 PRO-TIPO         PIC X(01).
                03 PRO-CPF-FORC     PIC 9(14).
                03 PRO-DATA         PIC 9(08).
                03 PRO-PRECO        PIC 9(08).
                03 PRO-QTDE         PIC 9(06).      				
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 W-CONT       	PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    	PIC X(01) VALUE SPACES.
       77 ST-ERRO   	PIC X(02) VALUE "00". 
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS         	PIC X(50) VALUE SPACES.
       77 LIMPA      	PIC X(50) VALUE SPACES.
      *
	   01 TXUNID.
	        03 UNID-SIGLA       PIC X(02) VALUE SPACES.
			03 UNID-TEXTO       PIC X(20) VALUE SPACES.
      *
       01 TBUNID.
            03 FILLER      PIC X(22) VALUE "DZDUZIA".
            03 FILLER      PIC X(22) VALUE "GRGRAMA".
            03 FILLER      PIC X(22) VALUE "HRHORA".
            03 FILLER      PIC X(22) VALUE "KGQUILOGRAMA".
            03 FILLER      PIC X(22) VALUE "LTLITRO".
            03 FILLER      PIC X(22) VALUE "MTMETRO".
            03 FILLER      PIC X(22) VALUE "M2METRO QUADRADO".
            03 FILLER      PIC X(22) VALUE "M3METRO CUBICO".
            03 FILLER      PIC X(22) VALUE "MIMILHEIRO".
            03 FILLER      PIC X(22) VALUE "PAPARES".
            03 FILLER      PIC X(22) VALUE "PCPECA".
            03 FILLER      PIC X(22) VALUE "QLQUILATE".
            03 FILLER      PIC X(22) VALUE "TNTONELADA".
            03 FILLER      PIC X(22) VALUE "UNUNIDADE".
            03 FILLER      PIC X(22) VALUE "WHQUILOWATT HORA".
      *
       01 TBUNIDR REDEFINES TBUNID.
             03 TABUNID      PIC X(22) OCCURS 15 TIMES.
       01 IND                PIC 9(02) VALUE ZEROS.	
      *
       01 TXTIPO.
	        03 TIPO-SIGLA       PIC X(01) VALUE SPACES.
			03 TIPO-TEXTO       PIC X(20) VALUE SPACES.
      *
       01 TBTIPO.
            03 FILLER      PIC X(21) VALUE "1SAUDE".
            03 FILLER      PIC X(21) VALUE "2ALIMENTOS".
            03 FILLER      PIC X(21) VALUE "3VESTUARIO".
            03 FILLER      PIC X(21) VALUE "4CALCADOS".
            03 FILLER      PIC X(21) VALUE "5BEBIDAS".
            03 FILLER      PIC X(21) VALUE "6BELEZA".
            03 FILLER      PIC X(21) VALUE "7TECNOLOGIA".
            03 FILLER      PIC X(21) VALUE "8TRANSPORTE".
            03 FILLER      PIC X(21) VALUE "9SERVICOS".
      *
       01 TBTIPOR REDEFINES TBTIPO.
             03 TABTIPO      PIC X(21) OCCURS 15 TIMES.
       01 IND2                PIC 9(02) VALUE ZEROS.

       	   
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "   ==========================  CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  "DE PRODUTO  ==========================  ".
           05  LINE 07  COLUMN 01 
               VALUE  "               CODIGO :".
           05  LINE 08  COLUMN 01 
               VALUE  "            DESCRICAO :".
		   05  LINE 09  COLUMN 01 
               VALUE  "              UNIDADE :".
           05  LINE 10  COLUMN 01 
               VALUE  "      TIPO DE PRODUTO :".
		   05  LINE 11  COLUMN 01 
               VALUE  "  CNPJ/CPF FORNECEDOR :".
           05  LINE 12  COLUMN 01 
               VALUE  "   DATA ULTIMA COMPRA :".
           05  LINE 13  COLUMN 01 
               VALUE  " PRECO UNITAR. COMPRA :".
           05  LINE 14  COLUMN 01 
               VALUE  "   QUANTIDADE ESTOQUE :".			   
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TPRO-COD
               LINE 07  COLUMN 25  PIC 99999.999
               USING  PRO-COD.
           05  TPRO-DESC
               LINE 08  COLUMN 25  PIC X(30)
               USING  PRO-DESC.
		   05  TPRO-UNID
               LINE 09  COLUMN 25  PIC X(02)
               USING  PRO-UNID.
           05  TUNID-TEXTO
               LINE 09  COLUMN 28  PIC X(20)
			   USING UNID-TEXTO.
           05  TPRO-TIPO
               LINE 10  COLUMN 25  PIC X(01)
               USING  PRO-TIPO.
           05  TTIPO-TEXTO
               LINE 10  COLUMN 27  PIC X(20)
			   USING TIPO-TEXTO.
           05  TPRO-CPF-FORC
               LINE 11  COLUMN 25  PIC 9(14)
               USING  PRO-CPF-FORC.
           05  TPRO-DATA
               LINE 12  COLUMN 25  PIC 9(08)
               USING  PRO-DATA.
           05  TPRO-PRECO
               LINE 13  COLUMN 25  PIC 9(08)
               USING  PRO-PRECO.
           05  TPRO-QTDE
               LINE 14  COLUMN 25  PIC 9(06)
               USING  PRO-QTDE.
       01 FTELA-PRO-UNID.
          05 LINE 05   COLUMN 55
             VALUE " UNIDADE DE MEDIDA".		  
          05 LINE 07   COLUMN 55
		     VALUE " DZ-DUZIA".
          05 LINE 08   COLUMN 55
		     VALUE " GR-GRAMA".
          05 LINE 09   COLUMN 55
		     VALUE " HR-HORA".
          05 LINE 10   COLUMN 55
		     VALUE " KG-QUILOGRAMA".
          05 LINE 11   COLUMN 55
		     VALUE " LT-LITRO".
          05 LINE 12   COLUMN 55
		     VALUE " MT-METRO".
          05 LINE 13   COLUMN 55
		     VALUE " M2-METRO QUADRADO".
          05 LINE 14   COLUMN 55
		     VALUE " M3-METRO CUBICO".
          05 LINE 15   COLUMN 55
		     VALUE " MI-MILHEIRO".
          05 LINE 16   COLUMN 55
		     VALUE " PA-PARES".
          05 LINE 17   COLUMN 55
		     VALUE " PC-PECA".
          05 LINE 18   COLUMN 55
		     VALUE " QL-QUILATE".
          05 LINE 19   COLUMN 55
		     VALUE " TN-TONELADA".
          05 LINE 20   COLUMN 55
		     VALUE " UN-UNIDADE".
          05 LINE 21   COLUMN 55
		     VALUE " WH-QUILOWATT HORA".
       01 FTELA-PRO-TIPO.
          05 LINE 05   COLUMN 55
             VALUE " TIPO DE PRODUTO  ".
          05 LINE 07   COLUMN 55
		     VALUE " 1-SAUDE          ".
          05 LINE 08   COLUMN 55
		     VALUE " 2-ALIMENTOS      ".
          05 LINE 09   COLUMN 55
		     VALUE " 3-VESTUARIO      ".
          05 LINE 10   COLUMN 55
		     VALUE " 4-CALCADOS       ".
          05 LINE 11   COLUMN 55
		     VALUE " 5-BEBIDAS        ".
          05 LINE 12   COLUMN 55
		     VALUE " 6-BELEZA         ".
          05 LINE 13   COLUMN 55
		     VALUE " 7-TECNOLOGIA     ".
          05 LINE 14   COLUMN 55
		     VALUE " 8-TRANSPORTE     ".
          05 LINE 15   COLUMN 55
		     VALUE " 9-SERVICOS       ".
          05 LINE 16   COLUMN 55
		     VALUE "                  ".
          05 LINE 17   COLUMN 55
		     VALUE "                  ".
          05 LINE 18   COLUMN 55
		     VALUE "                  ".
          05 LINE 19   COLUMN 55
		     VALUE "                  ".
          05 LINE 20   COLUMN 55
		     VALUE "                  ".
          05 LINE 21   COLUMN 55
		     VALUE "                  ".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQPRO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQPRO
                      CLOSE ARQPRO
                      MOVE "* ARQ PRODUTO SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQ PRODUTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       R01.
                MOVE SPACES TO TXUNID TXTIPO. 
                MOVE SPACES TO PRO-DESC PRO-UNID PRO-TIPO.
				MOVE ZEROS  TO PRO-COD.
				MOVE ZEROS  TO PRO-CPF-FORC.
                MOVE ZEROS  TO PRO-DATA PRO-PRECO PRO-QTDE.
                DISPLAY TELAPRO.
       R02.
                ACCEPT TPRO-COD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQPRO
                   GO TO ROT-FIM.
                IF PRO-COD = ZEROS 
                   MOVE "*** CODIGO NAO PODE SER ZEROS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R02.
       LER-COD01.
                MOVE 0 TO W-SEL
                READ ARQPRO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM R04A THRU R04B
					  PERFORM R05A THRU R05B
                      DISPLAY TELAPRO
                      MOVE "*** CODIGO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       R03.
                ACCEPT TPRO-DESC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R02.
                IF PRO-DESC = SPACES 
                   MOVE "DESC. DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R03.
       R04.
                DISPLAY FTELA-PRO-UNID
				ACCEPT TPRO-UNID
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R03.
       R04A.
	           MOVE 1 TO IND.
       R04B.
               MOVE TABUNID (IND) TO TXUNID
			   IF PRO-UNID NOT = UNID-SIGLA
                       ADD 1 TO IND
                       IF IND > 15
                          MOVE "UNIDADE NAO EXISTE" TO MENS
                          PERFORM ROT-MENS THRU ROT-MENS-FIM
                          GO TO R04
				       ELSE
                          GO TO R04B
			   ELSE
                     DISPLAY TUNID-TEXTO.
      *
       R05.
                DISPLAY FTELA-PRO-TIPO
                ACCEPT TPRO-TIPO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R04.
       R05A.
                MOVE 1 TO IND2.
       R05B.
               MOVE TABTIPO (IND2) TO TXTIPO
               IF PRO-TIPO NOT = TIPO-SIGLA
                       ADD 1 TO IND2
                       IF IND2 > 9
                          MOVE "TIPO NAO EXISTE" TO MENS
                          PERFORM ROT-MENS THRU ROT-MENS-FIM
                          GO TO R05
                       ELSE
                          GO TO R05B
               ELSE 
                     DISPLAY TTIPO-TEXTO.
      *					 
       R06.
	            ACCEPT TPRO-CPF-FORC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R05.
                IF PRO-CPF-FORC = ZEROS 
                   MOVE "CPF FORC. DEVE SER DIF. DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R06.
       R07.
                ACCEPT TPRO-DATA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R06.
                IF PRO-DATA = ZEROS 
                   MOVE "DATA DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R07.
       R08.
                ACCEPT TPRO-PRECO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R07.
                IF PRO-PRECO = ZEROS
                   MOVE "PRECO DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R08.
       R09.
	            ACCEPT TPRO-QTDE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R08.
                IF PRO-QTDE = ZEROS 
                   MOVE "QUANTID. DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R09.
				IF W-SEL = 1
							  GO TO ALT-OPC. 							  
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R09.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR01.
                WRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01.
                IF ST-ERRO = "22"
                      MOVE "*** PRODUTO JA EXISTE ***     " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
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
                DELETE ARQPRO RECORD
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
                IF W-ACT = 02 GO TO R09.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R01.
                MOVE "ERRO NA ALTERACAO DO REGISTRO PRODUTO"   TO MENS
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
