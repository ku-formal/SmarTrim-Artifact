contract Test{
  uint i=0;
  uint j=0;
  uint k=0;

  uint x=0;
  uint y=0;
  uint z=0;

  function loop1 () public{
    for (i=0; i<1000;i++){
    }
  }
 

  function loop2 () public{
    for (i=0;i<1000;i++){
      for (j=0;j<1000;j++){
	for (k=0;k<500;k++){ }
      }
    }
  }


  function loop3 () public{
    for (i=0;i<1000;i++){
      if (j==500){
        continue;
      }
      else { k++; }
    }
  }


  function loop4 () public{
    for (i=0;i<1000;i++){
      if (j==500){
	break;
      }
      else { k++; }
    }
  }


  function loop5 () public{
    for (i=0;i<1000;i++){
      if (i>=300 && i<=400) {
	continue;
      }
      j++;
      k++;
    }
  }
  

  function loop6 () public{
    for (i=0; i<100; i++){
      j++;
      if (i<50 || j<50) revert();
      k++;
    }
  }
  

  function loop7 () public{
    for (i=0; i<100; i++){
      require (j>=5);
      k++;
    }
  }
  

  function loop8 () public{
    require (j>=5);
    for (i=0; i<100; i++){
      k++;
    }
  }
  

  function loop9 () public{
    for (i=0; i<100; i++){
      j++;
      if (x>y && y>z) z=3;
      else if (x>10 && y<10) z=5;
    }
  }
  

  function loop10 () public{
    for (i=0; i<100; i++){
      if (x>0 && x<50) {
	if (y>=99){
	  if(z==0) break;
	  else z--;
	}
	j++;
      }
      else if (z==50) z++;
    }
    
    if (x==30) x++;
  }
 

  function loop11 () public returns (uint){
    for (i=0; i<100; i++){
      if (x>y)
	return 1;
      else if (x==y)
	return 2;
    }
    
    if (j<k) return 3;
    else return 4;
  }


  function loop12 () public returns (uint){
    for (i=0; i<100; i++){
      if (x==y){
	while (j>=100 && j<=200){
	  z--;
	}

	if (z<50) return z;
      }
    }
  }


  function dowhile1 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
    } while (n>0);
  }


  function dowhile2 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      for (uint i=0;i<100;i++) { }
    } while (n>0);
  }


  function dowhile3 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      for (uint i=0;i<100;i++) {
	uint p = 0;
	do {
	  p++;
	} while (p<10);
      }
    } while (n>0);
  }


  function dowhile4 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      if (m==0){
	break;
      }
      m = m-1;
    } while (n>0);
  }


  function dowhile5 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      for (uint i=0;i<100;i++) {
	if (i==10) {
	  break;
	}
      }
    } while (n>0);
  }


  function dowhile6 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      if (m==0){
	continue;
      }
      m = m-1;
    } while (n>0);
  }


  function dowhile7 (uint n, uint m) public {
    do {
      n = n-1;
      m = m-1;
      uint p = 0;
      do {
	p++;
	if (p<5){
	  break;
	}
      } while (p<=10);
      
      if (m==0){
	continue;
      }

      m = m-1;
    } while (n>0);
  }


  function noloop1 () public {
    for (i=0; i<1000;i++){ // should not be considered as a loop header
      return;
    }
  }
 

  function noloop2 () public {
    for (i=0;i<1000;i++){
      for (j=0;j<1000;j++){ // should not be considered as a loop header
	return;
	for (k=0;k<500;k++){
	}
      }
    }
  }
 

  function noloop3 () public {
    for (i=0;i<1000;i++){
      for (j=0;j<1000;j++){ // should not be considered as a loop header
	break;
	for (k=0;k<500;k++){
	}
      }
    }
  }
 

  function cond1() public{
    require (i>=300 && i<=400);
  }


  function cond2() public{
    require ((j<=400 || k>=5) && i>=300);
  }
  
  
  function cond3() public{
    require (! (i>=300 && i<=400));
  }
  
  
  function cond4() public{
    require (! (i>=300 || i<=400));
  }


  function return1 () public returns (uint){
    i = 100;
    return 5;
    j=5;
  }
 

  function return2 () public returns (uint){
    i = 100;
    return 5;
    j = 8;
    return 10;
    k = 99;
  }
}
