*> \brief <b> DSYEV computes the eigenvalues and, optionally, the left and/or right eigenvectors for SY matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> Download DSYEV + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dsyev.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dsyev.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dsyev.f">
*> [TXT]</a>
*
*  Definition:
*  ===========
*
*       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          JOBZ, UPLO
*       INTEGER            INFO, LDA, LWORK, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DSYEV computes all eigenvalues and, optionally, eigenvectors of a
*> real symmetric matrix A.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] JOBZ
*> \verbatim
*>          JOBZ is CHARACTER*1
*>          = 'N':  Compute eigenvalues only;
*>          = 'V':  Compute eigenvalues and eigenvectors.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored;
*>          = 'L':  Lower triangle of A is stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA, N)
*>          On entry, the symmetric matrix A.  If UPLO = 'U', the
*>          leading N-by-N upper triangular part of A contains the
*>          upper triangular part of the matrix A.  If UPLO = 'L',
*>          the leading N-by-N lower triangular part of A contains
*>          the lower triangular part of the matrix A.
*>          On exit, if JOBZ = 'V', then if INFO = 0, A contains the
*>          orthonormal eigenvectors of the matrix A.
*>          If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
*>          or the upper triangle (if UPLO='U') of A, including the
*>          diagonal, is destroyed.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] W
*> \verbatim
*>          W is DOUBLE PRECISION array, dimension (N)
*>          If INFO = 0, the eigenvalues in ascending order.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of the array WORK.  LWORK >= max(1,3*N-1).
*>          For optimal efficiency, LWORK >= (NB+2)*N,
*>          where NB is the blocksize for DSYTRD returned by ILAENV.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  if INFO = i, the algorithm failed to converge; i
*>                off-diagonal elements of an intermediate tridiagonal
*>                form did not converge to zero.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup heev
*
*  =====================================================================

      SUBROUTINE dsyev( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
*
*  -- LAPACK driver routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      parameter( zero = 0.0d0, one = 1.0d0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE,
     $                   LLWORK, LWKOPT, NB
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           lsame, ilaenv, dlamch, dlansy
*     ..
*     .. External Subroutines ..
      EXTERNAL           dlascl, dorgtr, dscal, dsteqr, dsterf,
     $                   dsytrd,
     $                   xerbla
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          max, sqrt
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      wantz = lsame( jobz, 'V' )
      lower = lsame( uplo, 'L' )
      lquery = ( lwork.EQ.-1 )
*
      info = 0
      IF( .NOT.( wantz .OR. lsame( jobz, 'N' ) ) ) THEN
         info = -1
      ELSE IF( .NOT.( lower .OR. lsame( uplo, 'U' ) ) ) THEN
         info = -2
      ELSE IF( n.LT.0 ) THEN
         info = -3
      ELSE IF( lda.LT.max( 1, n ) ) THEN
         info = -5
      END IF
*
      IF( info.EQ.0 ) THEN
         nb = ilaenv( 1, 'DSYTRD', uplo, n, -1, -1, -1 )
         lwkopt = max( 1, ( nb+2 )*n )
         work( 1 ) = lwkopt
*
         IF( lwork.LT.max( 1, 3*n-1 ) .AND. .NOT.lquery )
     $      info = -8
      END IF
*
      IF( info.NE.0 ) THEN
         CALL xerbla( 'DSYEV ', -info )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( n.EQ.0 ) THEN
         RETURN
      END IF
*
      IF( n.EQ.1 ) THEN
         w( 1 ) = a( 1, 1 )
         work( 1 ) = 2
         IF( wantz )
     $      a( 1, 1 ) = one
         RETURN
      END IF
*
*     Get machine constants.
*
      safmin = dlamch( 'Safe minimum' )
      eps = dlamch( 'Precision' )
      smlnum = safmin / eps
      bignum = one / smlnum
      rmin = sqrt( smlnum )
      rmax = sqrt( bignum )
*
*     Scale matrix to allowable range, if necessary.
*
      anrm = dlansy( 'M', uplo, n, a, lda, work )
      iscale = 0
      IF( anrm.GT.zero .AND. anrm.LT.rmin ) THEN
         iscale = 1
         sigma = rmin / anrm
      ELSE IF( anrm.GT.rmax ) THEN
         iscale = 1
         sigma = rmax / anrm
      END IF
      IF( iscale.EQ.1 )
     $   CALL dlascl( uplo, 0, 0, one, sigma, n, n, a, lda, info )
*
*     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
*
      inde = 1
      indtau = inde + n
      indwrk = indtau + n
      llwork = lwork - indwrk + 1
      CALL dsytrd( uplo, n, a, lda, w, work( inde ), work( indtau ),
     $             work( indwrk ), llwork, iinfo )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     DORGTR to generate the orthogonal matrix, then call DSTEQR.
*
      IF( .NOT.wantz ) THEN
         CALL dsterf( n, w, work( inde ), info )
      ELSE
         CALL dorgtr( uplo, n, a, lda, work( indtau ),
     $                work( indwrk ),
     $                llwork, iinfo )
         CALL dsteqr( jobz, n, w, work( inde ), a, lda,
     $                work( indtau ),
     $                info )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( iscale.EQ.1 ) THEN
         IF( info.EQ.0 ) THEN
            imax = n
         ELSE
            imax = info - 1
         END IF
         CALL dscal( imax, one / sigma, w, 1 )
      END IF
*
*     Set WORK(1) to optimal workspace size.
*
      work( 1 ) = lwkopt
*
      RETURN
*
*     End of DSYEV
*

      END
