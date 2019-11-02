! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM METOД ПОКООРДИНАТНОГО СПУСКА (AP1)
PROGRAM VAP1
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTA
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ)
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /AP11/ X1
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1 PABHA N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ  MACCИBA X1 = N
    REAL(8),DIMENSION(2)::X1
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=0.
    X(2)=0.
    ! ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    A(1)=-10.
    A(2)=-10.
    ! ПPABЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE
    B(1)=10.0
    B(2)=10.0
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0 
    PAR(Q+1)=0.000000001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO ИЗMEHEHИЮ ФУHKЦИOHAЛA
    PAR(Q+2)=5000        ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB, KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0           ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=1.0         ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+5)=0.6         ! MAЖOPAHTA APMИЙO
    PAR(Q+6)=0.5         ! KOЭФФИЦИEHT УMEHЬШEHИЯ ШAГA
    PAR(Q+7)=0.45        ! MAЖOPAHTA ГOЛДCTEЙHA
    PAR(Q+8)=6           ! HOMEP BEPCИИ METOДA ( = 1,2,3,4,5 ИЛИ 6 )
    PAR(Q+9)=0.00001     ! "HEOПPEДEЛEHHOCTЬ" ЗAДAHИЯ ГPAHИЦ ПAPAЛЛEЛEПИПEДA
    PAR(Q+10)=1          ! ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )
    PAR(Q+11)=0.00001    ! ШAГ ДИФФEPEHЦИPOBAHИЯ
    PAR(Q+12)=1          ! ЧИCЛO УДAЧHЫX ШAГOB, ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+13)=3          ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
! ВЫЗОВ МЕТОДА
    CALL AP1(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VAP1
