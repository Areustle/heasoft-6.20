awk '
#  Be careful: File name should not start with a digit. 
BEGIN {  
         for(l=1;l<=1; l++){
          fld[++nf]=ARGV[l];
          ARGV[l]=""
         }
         if(l>=ARGC){
          ARGV[ARGC++] ="-"
         }

        ncut=fld[1];
     }

# Get the various columns into arrays
 { if($0~/maximum/ && $0~/image/){print $7*ncut*0.01}
  }
' $*

