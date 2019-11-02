! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM METOД CЛУЧАЙНОГО ПОИСКА (AP6)
PROGRAM VAP6
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
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
    COMMON /AP61/ Z1
    COMMON /AP62/ XM
    COMMON /AP63/ W
    COMMON /AP64/ WM
    COMMON /AP65/ X1
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ BCEX MACCИBOB = N
    REAL(8),DIMENSION(2)::Z1,XM,W,WM,X1
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
    PAR(1)=0.0001   ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ
    PAR(2)=500      ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ
    PAR(3)=0        ! BЫXOДHOЙ ПAPAMETP
    PAR(4)=0.1      ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(5)=1.5      ! KOЭФФИЦИEHT УBEЛИЧEHИЯ ШAГA CПУCKA
    PAR(6)=0.903602 ! KOЭФФИЦИEHT УMEHЬШEHИЯ ШAГA CПУCKA
    PAR(7)=5.0      ! ПAPAMETP ИЗMEHEHИЯ ПЛOTHOCTИ PACПPEДEЛEHИЯ
    PAR(8)=10       ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE MEHЯETCЯ ПЛOTHOCTЬ PACПPEДEЛEHИЯ CЛУЧAЙHЫX ЧИCEЛ
    PAR(9) =20      ! OБЩEE ЧИCЛO ШAГOB,ЧEPEЗ KOTOPOE MEHЯETCЯ ПЛOTHOCTЬ PACПPEДEЛEHИЯ CЛУЧAЙHЫX ЧИCEЛ
    PAR(10)= 13579  ! HEЧETHOE ЧИCЛO ИЗ ИHTEPBAЛA (1;67108863) ДЛЯ ЗAПУCKA ГEHEPATOPA CЛУЧAЙHЫX ЧИCEЛ
    PAR(11)=1       ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(12)=3       ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ: OT 0 ДO 3
! ВЫЗОВ МЕТОДА
    CALL AP6(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VAP6
