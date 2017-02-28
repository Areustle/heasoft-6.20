%ahtestpar = (
  'task' => 'testahlog',
  'taskarg' => [
    'stacktrace'
  ],
  'testdescription' => 'Test \'stacktrace\' option',
  'expectedtaskstatus' => 0,
  'inputdatafile' => [
    'syspfiles/testahlog.par'
  ],
  'expectedoutputfile' => [
    'pfiles/testahlog.par',
    'syspfiles/testahlog.par',
    'testahlog-log',
    'testahlog.log'
  ]
);
