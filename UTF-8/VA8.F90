C
C              TECTИPOBAHИE METOДOB ИЗ ПAKETA БM
C              MOДИФИЦИPOBAHHЫЙ METOД HЬЮTOHA  ( A8  )
C
C  BHИMAHИE: ДЛЯ OБPAЩEHИЯ MATPИЦЫ ИCПOЛЬЗУETCЯ
C            ПOДПPOГPAMMA DMINV.ПOЭTOMУ HEOБXOДИMO
C            ПOДKЛЮЧEHИE БИБЛИOTEKИ SSPLIB
C
C        OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
C
C      PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
         REAL *8 X(2),A(2),B(2),G1(2)
         REAL *8 PAR(40),Y
         REAL *8 F,FNLP
         EXTERNAL F,FNLP,GRAD,AGS
         INTEGER Q
C
C           OПИCAHИE OБЩИX OБЛACTEЙ METOДA
C
         COMMON /A829/ M1/A81  /HES /A82  / P1 /A83  /  P2
     *          /A84 / XV /A85 / FV /A86 / XT /A87 / TR /A88 /L1
C       PAЗMEPHOCTЬ  MACCИBA HES  = N*N
         REAL *8 HES (2 ,2)
C       PAЗMEPHOCTЬ  MACCИBA P2   = 3*N
         REAL *8 P2  (3 ,2)
C       PAЗMEPHOCTЬ  BCEX MACCИBOB  = N
         REAL *8  P1( 2 ),XV( 2 ),L1( 2 ),M1( 2 ),
     *           FV( 2 ),XT( 2 ),TR( 2 )
C
C           XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
C
C     N - PAЗMEPHOCTЬ ЗAДAЧИ
C     X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
C     A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
C     B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
C     F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ
C         ЗHAЧEHИЯ KPИTEPИЯ
C     GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ
C         BЫЧИCЛEHИЯ ГPAДИEHTA  ( ИCПOЛЬЗУETCЯ CBOЯ
C         BCTPOEHHAЯ ПPOГPAMMA BЫЧИCЛEHИЯ ГPAДИEHTA )
C     AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA
C           MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ )
C     Y - ЗHAЧEHИE KPИTEPИЯ
C     G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
C     Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
C     PAR - BEKTOP ПAPAMETPOB METOДA
C     FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
C
C         ИCXOДHЫE ДAHHЫE ЗAДAЧИ
C
C     PAЗMEPHOCTЬ ЗAДAЧИ
         N=2
C     HAЧAЛЬHAЯ TOЧKA
         X(1)=  1.2
         X(2)=  1.0
C
C    ЗAДAHИE ПAPAMETPOB METOДA
C
         Q=0
C     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA
         PAR(Q+1)=0.00001
C     MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
         PAR(Q+2)=25
C     BЫXOДHOЙ ПAPAMETP
         PAR(Q+3)=0
C     ПPИЗHAK HAЧAЛЬHOЙ OПPEДEЛEHHOCTИ ПAPAMETPOB Y И G1
C                          ( 0,1 ИЛИ 2 )
         PAR(Q+4)=0
C     HAЧAЛЬHЫЙ ШAГ CПУCKA
         PAR(Q+5)=0.001
C     MAЖOPAHTA ГOЛДCTEЙHA
         PAR(Q+6)=0.45
C     ШAГ KOHEЧHO-PAЗHOCTHOЙ AППPOKCИMAЦИИ ПPOИЗBOДHЫX
         PAR(Q+7)=0.00001
C     ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ
C     ИHФOPMAЦИЮ
         PAR(Q+8)=1
C     CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
         PAR(Q+9)=3
C
         CALL A8 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
         END
