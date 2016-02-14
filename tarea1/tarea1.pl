perrin(0,3).
perrin(1,0).
perrin(2,2).
perrin(N,M) :- N > 2,
							 N2 is N-3, perrin(N2,M2),
							 N1 is N-2, perrin(N1,M1),
							 M is M2+M1.
