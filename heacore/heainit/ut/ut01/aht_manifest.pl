%ahtestpar = (
  'task' => 'test_heainit',
  'testdescription' => 'Test special conditions.',
  'expectedtaskstatus' => 0,
  'inputdatafile' => [
    'syspfiles/test_heainit.par'
  ],
  'expectedoutputfile' => [
    'empty.log',
    'empty.par',
    'pfiles/test_heainit.par',
    'syspfiles/test_heainit.par',
    'test_heainit-log'
  ]
);
