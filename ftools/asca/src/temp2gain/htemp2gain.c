int MAIN_;              /* for sun fortran bug */

int temp2gain(void);

int
main(int argc, char **argv)
{
        OpenDefaultPF(argc, argv);
        temp2gain();
        CloseDefaultPF();
        return 0;
} 


