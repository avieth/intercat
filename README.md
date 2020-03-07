# intercat

This is an interactive program for moving bytes between files.

Where you would use

```sh
cat file_1 file_2 > file_3
```

you could now use

```
./intercat
> add lazy source file_1
> add lazy source file_2
> add lazy sink file_3
> commit
```

Why would you want such a complicated beast for such a simple task?
It was invented for one particular use case: there is a program which forever
reads from stdin, and the user wants to move data from her text editor into
that stdin stream. There are plenty of choices of intermediary: a socket via
netcat, a named pipe (FIFO), even directly from a simple character device like
an old-school COM port. That's all to say, we've got a scenario like this:

```sh
# On machine A
nc -lk IP4_ADDRESS PORT | process

# On machine B (could be the same as A)
mkfifo editor_sink
# Never stop reading the FIFO (even if it closes) and throw it all into the
# netcat socket.
nc IP4_ADDRESS PORT < ifinicat editor_sink &

touch editor_source
# Open up the editor in some other terminal or in a graphical interface.
editor editor_source

intercat
> add lazy source editor_source
> add lazy sink editor_sink
# The user chooses when to commit, which copies the edited file to the
# fifo, and then over the socket, and ultimately to the process.
...
```

# infinicat

Write the contents of a file to stdout, over and over again. Useful when
dealing with a named pipe (FIFO). A FIFO gives EOF when it has no more writers,
and that can be problematic when the FIFO is piped to the stdin of some process.

```sh
mkfifo my_fifo
long_running_process < my_fifo &
echo hello > my_fifo
# long_running_process now has no more stdin

long_running_process < infinicat my_fifo &
echo hello > my_fifo
echo world > my_fifo
```
