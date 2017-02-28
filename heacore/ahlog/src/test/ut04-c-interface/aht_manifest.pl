%ahtestpar = (
  'task' => 'testcahlog',
  'taskarg' => [
    'debug=yes',
    'chatter=3',
    'logfile=output/testcahlog.log'
  ],
  'testdescription' => 'Test C code that uses ahlog.',
  'expectedtaskstatus' => 0,
  'inputdatafile' => [
    'syspfiles/testcahlog.par'
  ],
  'expectedoutputfile' => [
    'output/testcahlog.log',
    'pfiles/testcahlog.par',
    'syspfiles/testcahlog.par',
    'testcahlog-log'
  ],
  'ignore_non_fits_diff' => 1
);
