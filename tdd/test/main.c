#include "vendor/unity.h"
#include <setjmp.h>
#include <stdio.h>

char MessageBuffer[50];

void setUp(void)
{
}

void tearDown(void)
{
}

void resetTest(void)
{
  tearDown();
  setUp();
}

void test_the_framework_works(void)
{
  TEST_ASSERT_EQUAL(0, 0);
}

int main(void)
{
  UnityBegin("test/TestProductionCode.c");

  RUN_TEST(test_the_framework_works, 20);

  UnityEnd();
  return 0;
}
