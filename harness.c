#include "Unity/src/unity.h"

int get100();

void test_get(void)
{
TEST_ASSERT_EQUAL(100, get100());
}

int main(void)
{
UNITY_BEGIN();
RUN_TEST(test_get);
return UNITY_END();
}
