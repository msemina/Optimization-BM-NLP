!
!     ..................................................................
!
!        SUBROUTINE DMINV
!
!        PURPOSE
!           INVERT A MATRIX
!
!        USAGE
!           CALL DMINV(A,N,L,M)
!
!        DESCRIPTION OF PARAMETERS
!           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
!               RESULTANT INVERSE.
!           N - ORDER OF MATRIX A
!           D - RESULTANT DETERMINANT
!           L - WORK VECTOR OF LENGTH N
!           M - WORK VECTOR OF LENGTH N
!
!        REMARKS
!           MATRIX A MUST BE A GENERAL MATRIX
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT
!           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
!           THE MATRIX IS SINGULAR.
!
!     ..................................................................
!
SUBROUTINE DMINV(A,N,D,L,M)
    REAL(8),DIMENSION(1)::A,L,M
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        ! IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
    REAL(8)::D,BIGA,HOLD
!
!        THE ! MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT
!        68 MUST BE CHANGED TO DABS.
!
!        ...............................................................
!
!        SEARCH FOR LARGEST ELEMENT
!
    D=1.0
    NK=-N
    DO K=1,N
        NK=NK+N
        L(K)=K
        M(K)=K
        KK=NK+K
        BIGA=A(KK)
        DO J=K,N
            IZ=N*(J-1)
            DO I=K,N
                IJ=IZ+I
                IF( DABS(BIGA)- DABS(A(IJ)) < 0 )THEN
                    BIGA=A(IJ)
                    L(K)=I
                    M(K)=J
                END IF
            END DO
        END DO
!
!        INTERCHANGE ROWS
!
        J=L(K)
        IF(J-K>0)THEN
            KI=K-N
            DO I=1,N
                KI=KI+N
                HOLD=-A(KI)
                JI=KI-K+J
                A(KI)=A(JI)
                A(JI) =HOLD
            END DO
        END IF
!
!        INTERCHANGE COLUMNS
!
        I=M(K)
        IF(I-K>0)THEN
            JP=N*(I-1)
            DO J=1,N
                JK=NK+J
                JI=JP+J
                HOLD=-A(JK)
                A(JK)=A(JI)
                A(JI) =HOLD
            END DO
        END IF
!
!        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
!        CONTAINED IN BIGA)
!
        IF(BIGA==0)THEN
            D=0.0
            RETURN
        END IF
        DO I=1,N
            IF(I-K/=0)THEN
                IK=NK+I
                A(IK)=A(IK)/(-BIGA)
            END IF
        END DO
!
!        REDUCE MATRIX
!
        DO I=1,N
            IK=NK+I
            HOLD=A(IK)
            IJ=I-N
            DO J=1,N
                IJ=IJ+N
                IF(I-K/=0)THEN
                    IF(J-K/=0)THEN
                        KJ=IJ-I+K
                        A(IJ)=HOLD*A(KJ)+A(IJ)
                    END IF
                END IF
            END DO
        END DO
!
!        DIVIDE ROW BY PIVOT
!
        KJ=K-N
        DO J=1,N
            KJ=KJ+N
            IF(J-K/=0)A(KJ)=A(KJ)/BIGA
        END DO
!
!        PRODUCT OF PIVOTS
!
        D=D*BIGA
!
!        REPLACE PIVOT BY RECIPROCAL
!
        A(KK)=1.0/BIGA
    END DO
!
!        FINAL ROW AND COLUMN INTERCHANGE
!
    K=N
    DO
        K=(K-1)
        IF(K<=0)EXIT
        I=L(K)
        IF(I-K>0)THEN
            JQ=N*(K-1)
            JR=N*(I-1)
            DO J=1,N
                JK=JQ+J
                HOLD=A(JK)
                JI=JR+J
                A(JK)=-A(JI)
            END DO
            A(JI)=HOLD
        END IF
        J=M(K)
        IF(J-K>0)THEN
            KI=K-N
            DO I=1,N
                KI=KI+N
                HOLD=A(KI)
                JI=KI-K+J
                A(KI)=-A(JI)
            END DO
            A(JI)=HOLD
        END IF
    END DO
    RETURN
END SUBROUTINE DMINV
