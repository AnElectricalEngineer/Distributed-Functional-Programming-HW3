-module(tests).
-compile(export_all).

test1()->
	io:format('let\'s start with checking that we have three servers and that they are supervised\n'),
	loadBalance:startServers(),
	
	true = is_pid(whereis(server1)),
	true = is_pid(whereis(server2)),
	true = is_pid(whereis(server3)),
	
	exit(whereis(server1), normal),
	exit(whereis(server2), normal),
	exit(whereis(server3), normal),
	
	timer:sleep(250),
	
	true = is_pid(whereis(server1)),
	true = is_pid(whereis(server2)),
	true = is_pid(whereis(server3)),
	
	loadBalance:stopServers().

receive_message(MsgRef, Value)->
	receive
		{MsgRef,Value} -> good
		after 10000 -> exit(bad)
	end.
	
test2()->
	io:format('let\'s check that when we send a job we get ok and we get the result in a message\n'),
	loadBalance:startServers(),
	MsgRef = make_ref(),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(1000), 2+1 end, MsgRef),
	receive_message(MsgRef, 3),
	loadBalance:stopServers().
	
test3()->
	io:format('now let\'s check that we spread the jobs even\n'),
	loadBalance:startServers(),
	MsgRef1 = make_ref(),
	MsgRef2 = make_ref(),
	MsgRef3 = make_ref(),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(2000), 2+1 end, MsgRef1),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(2000), 2+1 end, MsgRef2),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(2000), 2+1 end, MsgRef3),
	1 = loadBalance:numberOfRunningFunctions(1),
	1 = loadBalance:numberOfRunningFunctions(2),
	1 = loadBalance:numberOfRunningFunctions(3),
	receive_message(MsgRef1, 3),
	receive_message(MsgRef2, 3),
	receive_message(MsgRef3, 3),
	loadBalance:stopServers().	

test4()->
	io:format('and let\'s check that we can make multiple jobs at a time and return them to their right place\n'),
	loadBalance:startServers(),
	MsgRef1 = make_ref(),
	MsgRef2 = make_ref(),
	MsgRef3 = make_ref(),
	MsgRef4 = make_ref(),
	MsgRef5 = make_ref(),
	MsgRef6 = make_ref(),
	MsgRef7 = make_ref(),
	MsgRef8 = make_ref(),
	MsgRef9 = make_ref(),
	
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 1 end, MsgRef1),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 2 end, MsgRef2),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 3 end, MsgRef3),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 4 end, MsgRef4),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 5 end, MsgRef5),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 6 end, MsgRef6),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 7 end, MsgRef7),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 8 end, MsgRef8),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 9 end, MsgRef9),
	
	3 = loadBalance:numberOfRunningFunctions(1),
	3 = loadBalance:numberOfRunningFunctions(2),
	3 = loadBalance:numberOfRunningFunctions(3),
	
	receive_message(MsgRef1, 1),
	receive_message(MsgRef2, 2),
	receive_message(MsgRef3, 3),
	receive_message(MsgRef4, 4),
	receive_message(MsgRef5, 5),
	receive_message(MsgRef6, 6),
	receive_message(MsgRef7, 7),
	receive_message(MsgRef8, 8),
	receive_message(MsgRef9, 9),
	
	loadBalance:stopServers().
	
test5()->
	io:format('probably last. let\'s check that the servers hold the real number of running jobs\nprepare, this one is long\n'),
	loadBalance:startServers(),
	MsgRef1  = make_ref(),
	MsgRef2  = make_ref(),
	MsgRef3  = make_ref(),
	MsgRef4  = make_ref(),
	MsgRef5  = make_ref(),
	MsgRef6  = make_ref(),
	MsgRef7  = make_ref(),
	MsgRef8  = make_ref(),
	MsgRef9  = make_ref(),
	MsgRef10 = make_ref(),
	MsgRef11 = make_ref(),
	MsgRef12 = make_ref(),
	MsgRef13 = make_ref(),
	MsgRef14 = make_ref(),
	MsgRef15 = make_ref(),
	MsgRef16 = make_ref(),
	MsgRef17 = make_ref(),
	MsgRef18 = make_ref(),
	
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 1 end, MsgRef1),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 2 end, MsgRef2),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(3000), 3 end, MsgRef3),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 4 end, MsgRef4),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 5 end, MsgRef5),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 6 end, MsgRef6),
	
	2 = loadBalance:numberOfRunningFunctions(1),
	2 = loadBalance:numberOfRunningFunctions(2),
	2 = loadBalance:numberOfRunningFunctions(3),
	
	timer:sleep(3000),
	io:format('1 out of 4\n'),

	1 = loadBalance:numberOfRunningFunctions(1),
	1 = loadBalance:numberOfRunningFunctions(2),
	1 = loadBalance:numberOfRunningFunctions(3),
	
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 7 end,  MsgRef7),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 8 end,  MsgRef8),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 9 end,  MsgRef9),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 10 end, MsgRef10),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 11 end, MsgRef11),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 12 end, MsgRef12),	
	
	3 = loadBalance:numberOfRunningFunctions(1),
	3 = loadBalance:numberOfRunningFunctions(2),
	3 = loadBalance:numberOfRunningFunctions(3),
	
	timer:sleep(3000),
	io:format('2 out of 4\n'),
	
	2 = loadBalance:numberOfRunningFunctions(1),
	2 = loadBalance:numberOfRunningFunctions(2),
	2 = loadBalance:numberOfRunningFunctions(3),
	
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 13 end, MsgRef13),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 14 end, MsgRef14),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 15 end, MsgRef15),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 16 end, MsgRef16),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 17 end, MsgRef17),
	ok = loadBalance:calcFun(self(),fun() -> timer:sleep(6000), 18 end, MsgRef18),

	4 = loadBalance:numberOfRunningFunctions(1),
	4 = loadBalance:numberOfRunningFunctions(2),
	4 = loadBalance:numberOfRunningFunctions(3),
	
	timer:sleep(3000),
	io:format('3 out of 4\n'),
	
	2 = loadBalance:numberOfRunningFunctions(1),
	2 = loadBalance:numberOfRunningFunctions(2),
	2 = loadBalance:numberOfRunningFunctions(3),
	
	timer:sleep(3000),
	io:format('4 out of 4\n'),
	
	0 = loadBalance:numberOfRunningFunctions(1),
	0 = loadBalance:numberOfRunningFunctions(2),
	0 = loadBalance:numberOfRunningFunctions(3),

	receive_message(MsgRef1 ,  1),
	receive_message(MsgRef2 ,  2),
	receive_message(MsgRef3 ,  3),
	receive_message(MsgRef4 ,  4),
	receive_message(MsgRef5 ,  5),
	receive_message(MsgRef6 ,  6),
	receive_message(MsgRef7 ,  7),
	receive_message(MsgRef8 ,  8),
	receive_message(MsgRef9 ,  9),
	receive_message(MsgRef10, 10),
	receive_message(MsgRef11, 11),
	receive_message(MsgRef12, 12),
	receive_message(MsgRef13, 13),
	receive_message(MsgRef14, 14),
	receive_message(MsgRef15, 15),
	receive_message(MsgRef16, 16),
	receive_message(MsgRef17, 17),
	receive_message(MsgRef18, 18),
	
	loadBalance:stopServers().
	
	
run_all()->
	test1(),
	timer:sleep(250),
	test2(),
	timer:sleep(250),
	test3(),
	timer:sleep(250),
	test4(),	
	timer:sleep(250),
	test5(),
	io:format('we are done\n').