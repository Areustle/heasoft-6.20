	subroutine gtstat(value)
	integer*4 value(5)
C---
C **	** routine to get statistics
C---
C value(1) - Elapsed clock time (in units of 10msec).
C value(2) - Elapsed CPU time (in units of 10msec).
C value(3) - buffered I/O count
C value(4) - direct I/O count
C value(5) - page fault count

C **    ** call system dependent routine

	call getts (value)

	return
	end
