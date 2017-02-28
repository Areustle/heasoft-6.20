%ahtestpar = (
  'task' => 'testahlog',
  'taskarg' => [
    'class'
  ],
  'testdescription' => 'Test \'class\' option',
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
