!*************************************************************************************************!
!CAUTION:Recommend user to operate this program under a binarial ary, as the transferring between !
!two aries is not so stable. This program doesn't surport negative numberal converting, so when   !
!you do a substraction calculating, pleasr input the data in binary.                              !
!*************************************************************************************************!
PROGRAM binary
  IMPLICIT NONE
    CHARACTER(len=9)::bi1,bi2,bi3
	INTEGER::de1,de2
	INTEGER::i,j,k
	INTEGER::c
!Input data to be calculated.
	WRITE(*,*)'Choose an ary to input data, "1" for binary, and "2" for decimal.'
	READ(*,*)c
	SELECT CASE (c)
	  CASE (1)
	    WRITE(*,*)'The input should be a nine-byte character, complete higher digits with zero.'
	    READ(*,*)bi1,bi2
	  CASE (2)
	    READ(*,*)de1,de2
	    CALL binarize (bi1,de1)
	    CALL binarize (bi2,de2)
		WRITE(*,*)bi1
		WRITE(*,*)bi2
	  CASE DEFAULT
	    WRITE(*,*)'You can only input in binary or decimal format.'
	END SELECT
!Do arithmatic, choose addition or substraction.
	WRITE(*,*)'Choose what arithmatic you want to do, "1" for addition, and "2" for substraction.'
	READ(*,*)c
	SELECT CASE (c)
!addition
	  CASE (1)
	    CALL addition(bi1,bi2,bi3)
!Choosing output method.
	    WRITE(*,*)'Now choose a format to output data, "1" for binary, and "2" for decimal.'
	    READ(*,*)c
	    SELECT CASE (c)
	      CASE (1)
		  WRITE(*,'(1X,A34,2X,A9)')'After addition calculating you get',bi3
	      CASE (2)
		  CALL decimalize (bi3,de1)
		  WRITE(*,'(1X,A34,2X,I4)')'After addition calculating you get',de1
		  CASE DEFAULT
	      WRITE(*,*)'You can only output in binary or decimal format.'
		END SELECT
!substraction
	  CASE (2)
	    CALL substraction(bi1,bi2,bi3)
!Choosing output method.
		WRITE(*,*)'Now choose a format to output data, "1" for binary, and "2" for decimal.'
	    READ(*,*)c
	    SELECT CASE (c)
	      CASE (1)
		  WRITE(*,'(1X,A38,2X,A9)')'After substraction calculating you get',bi3
	      CASE (2)
		  CALL decimalize (bi3,de1)
		  WRITE(*,'(1X,A38,2X,I4)')'After substraction calculating you get',de1
		  CASE DEFAULT
	      WRITE(*,*)'You can only output in binary or decimal format.'
		END SELECT
	  CASE DEFAULT
	    WRITE(*,*)'No corresponding calculation!'
	END SELECT
END PROGRAM
!*************************************************************************************************!
!***Sub Routines***!
!*************************************************************************************************!
  SUBROUTINE addition (bi1,bi2,bi3)
    IMPLICIT NONE
	  CHARACTER,INTENT(in)::bi1*9,bi2*9
	  CHARACTER,INTENT(out)::bi3*9
	  INTEGER:: a,b,c
	  INTEGER:: i,j
	  DO i=9,1,-1
		IF (bi1(i:i)=='0') THEN 
		  a=0
		ELSE IF (bi1(i:i)=='1') THEN 
		  a=1
		END IF
		IF (bi2(i:i)=='0') THEN 
		  b=0
		ELSE IF (bi2(i:i)=='1') THEN 
		  b=1
		END IF
		  c=a+b
		IF (c==0) THEN 
		  bi3(i:i)='0'
		ELSE IF (c==1) THEN 
		  bi3(i:i)='1'
		ELSE IF (c==2) THEN
		  bi3(i:i)='2'
		END IF
	  END DO
	  DO j=9,2,-1
	    IF (bi3(j:j)=='2') THEN
		  bi3(j:j)='0'
		  IF (bi3(j-1:j-1)=='0') THEN
		    bi3(j-1:j-1)='1'
		  ELSE IF (bi3(j-1:j-1)=='1') THEN
		    bi3(j-1:j-1)='2'
		  ELSE IF (bi3(j-1:j-1)=='2') THEN
		    bi3(j-1:j-1)='3'
		  END IF
        ELSE IF (bi3(j:j)=='3') THEN
		  bi3(j:j)='1'
		  IF (bi3(j-1:j-1)=='0') THEN
		    bi3(j-1:j-1)='1'
		  ELSE IF (bi3(j-1:j-1)=='1') THEN
		    bi3(j-1:j-1)='2'
		  ELSE IF (bi3(j-1:j-1)=='2') THEN
		    bi3(j-1:j-1)='3'
		  END IF
		END IF
	  END DO
  END SUBROUTINE addition
!*************************************************************************************************!
!*************************************************************************************************!
  SUBROUTINE substraction (bi1,bi2,bi3)
    IMPLICIT NONE
	  CHARACTER,INTENT(inout)::bi1*9,bi2*9
	  CHARACTER,INTENT(out)::bi3*9
	  INTEGER:: a,b,c
	  INTEGER:: i,j
	  DO j=1,9
	    IF (bi2(j:j)=='1') THEN
		  bi2(j:j)='0'
		ELSE IF (bi2(j:j)=='0') THEN
		  bi2(j:j)='1'
		END IF
	  END DO
	  IF (bi2(9:9)=='0') THEN
	    bi2(9:9)='1'
	  ELSE IF (bi2(9:9)=='1') THEN
	    bi2(9:9)='2'
	  END IF
	  DO j=9,2,-1
	    IF (bi2(j:j)=='2') THEN
		  bi2(j:j)='0'
		  IF (bi2(j-1:j-1)=='0') THEN
		    bi2(j-1:j-1)='1'
		  ELSE IF (bi2(j-1:j-1)=='1') THEN
		    bi2(j-1:j-1)='2'
		  END IF
		END IF
	  END DO
      DO i=9,1,-1
		IF (bi1(i:i)=='0') THEN 
		  a=0
		ELSE IF (bi1(i:i)=='1') THEN 
		  a=1
		END IF
		IF (bi2(i:i)=='0') THEN 
		  b=0
		ELSE IF (bi2(i:i)=='1') THEN 
		  b=1
		END IF
		  c=a+b
		IF (c==0) THEN 
		  bi3(i:i)='0'
		ELSE IF (c==1) THEN 
		  bi3(i:i)='1'
		ELSE IF (c==2) THEN
		  bi3(i:i)='2'
		END IF
	  END DO
	  DO j=9,2,-1
	    IF (bi3(j:j)=='2') THEN
		  bi3(j:j)='0'
		  IF (bi3(j-1:j-1)=='0') THEN
		    bi3(j-1:j-1)='1'
		  ELSE IF (bi3(j-1:j-1)=='1') THEN
		    bi3(j-1:j-1)='2'
		  ELSE IF (bi3(j-1:j-1)=='2') THEN
		    bi3(j-1:j-1)='3'
		  END IF
        ELSE IF (bi3(j:j)=='3') THEN
		  bi3(j:j)='1'
		  IF (bi3(j-1:j-1)=='0') THEN
		    bi3(j-1:j-1)='1'
		  ELSE IF (bi3(j-1:j-1)=='1') THEN
		    bi3(j-1:j-1)='2'
		  ELSE IF (bi3(j-1:j-1)=='2') THEN
		    bi3(j-1:j-1)='3'
		  END IF
		END IF
	  END DO
	  DO j=1,9
	    IF (bi3(j:j)=='1') THEN
		  bi3(j:j)='0'
		ELSE IF (bi3(j:j)=='0') THEN
		  bi3(j:j)='1'
		END IF
	  END DO
	  IF (bi3(9:9)=='0') THEN
	    bi3(9:9)='1'
	  ELSE IF (bi3(9:9)=='1') THEN
	    bi3(9:9)='2'
	  END IF
	  DO j=9,2,-1
	    IF (bi3(j:j)=='2') THEN
		  bi3(j:j)='0'
		  IF (bi3(j-1:j-1)=='0') THEN
		    bi3(j-1:j-1)='1'
		  ELSE IF (bi3(j-1:j-1)=='1') THEN
		    bi3(j-1:j-1)='2'
		  END IF
		END IF
	  END DO
  END SUBROUTINE substraction
!*************************************************************************************************!
!*************************************************************************************************!
  SUBROUTINE decimalize (bi,de)
    IMPLICIT NONE
	  CHARACTER,INTENT(in)::bi*9
	  INTEGER,INTENT(out)::de
	  INTEGER::i
	  DO i=9,1,-1
	    IF (bi(i:i)=='0') THEN
		  de=de+0
		ELSE IF (bi(i:i)=='1') THEN
		  de=de+1*2**(9-i)
		END IF
	  END DO
  END SUBROUTINE decimalize
!*************************************************************************************************!
!*************************************************************************************************!
  SUBROUTINE binarize (bi,de)
    IMPLICIT NONE
	  INTEGER,INTENT(in)::de
	  CHARACTER,INTENT(out)::bi*9
	  INTEGER::i,a,temp
	  a=de
	  DO i=9,1,-1
	    temp=MOD(a,2)
		a=FLOOR(1.*a/2.)
		IF (temp==0) THEN
		  bi(i:i)='0'
		ELSE IF (temp==1) THEN
		  bi(i:i)='1'
		END IF
	  END DO
  END SUBROUTINE binarize