int MAIN_;              /* for sun fortran bug */

int corpileup(void);

int
main(int argc, char **argv)
{
        OpenDefaultPF(argc, argv);
        corpileup();
        CloseDefaultPF();
        return 0;
}


