// We can set the thread random seed
thisThread.randSeed = 666;

// Construct an array by calling this function 10 times
{ rand(10.0) }.dup(10);
