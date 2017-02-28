@.sub a
echo a
@=
@.endsub

@.sub b
@//b $%*
@.endsub

@.sub c
@.return -1
@.endsub

@.call a 111 22 333

@.call b

@.call c

@.sub abc
@=
@.call a $%*
@.call b $%*
@.call c $%*
@.endsub

@.call abc
@.call abc 1 22 333
