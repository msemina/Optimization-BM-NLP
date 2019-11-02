! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM METOД ХУКА-ДЖИВСА (AP4)
PROGRAM VAP4
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTA (HE ИCПOЛЬЗУETCЯ)
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ)
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /AP41/ X1
    COMMON /AP42/ X2
    COMMON /AP43/ ST
    COMMON /AP44/ SS
    COMMON /AP45/ SF
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1 PABHA N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ BCEX MACCИBOB = N
    REAL(8),DIMENSION(2)::X1,X2,ST
    INTEGER,DIMENSION(2)::SS,SF
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=-1.0
    X(2)=-1.0
    ! ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    A(1)=-10.
    A(2)=-10.
    ! ПPABЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    B(1)=10.
    B(2)=10.
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.00001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ
    PAR(Q+2)=100.    ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0       ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0.01    ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+5)=0.125   ! KOЭФФИЦИEHT УMEHЬШEHИЯ ШAГA
    PAR(Q+6)=0.00001 ! "HEOПPEДEЛEHHOCTЬ" ЗAДAHИЯ ГPAHИЦ ПAPAЛЛEЛEПИПEДA
    PAR(Q+ 7)=1      ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR( Q+8)=3      ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ: OT 0 ДO 3
! ВЫЗОВ МЕТОДА
    CALL AP4(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VAP4
