%ahtestpar = (
  'task' => 'testahlog',
  'taskarg' => [
    'function',
    'debug=yes',
    'logfile=output/testahlog.log'
  ],
  'testdescription' => 'Test \'function\' option',
  'expectedtaskstatus' => 0,
  'inputdatafile' => [
    'syspfiles/testahlog.par'
  ],
  'expectedoutputfile' => [
    'output/testahlog.log',
    'pfiles/testahlog.par',
    'syspfiles/testahlog.par',
    'testahlog-log'
  ],
  'ignore_non_fits_diff' => 1
);
