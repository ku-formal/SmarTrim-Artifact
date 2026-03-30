contract Test{
	uint gvar;

	function test(){
		gvar = 999;
		msg.sender.call.gas(100000).value (1 ether) ("bbb");
		msg.sender.call ("aaa");
		msg.sender.call.value (2 ether).gas (5000) ("ccc");
		msg.sender.call.value (2 ether).gas (5000).gas(100000000) ("ccc");
		msg.sender.call.value (2 ether).value(88888).gas (5000).gas(100000000) ("ccc");

	}
}
