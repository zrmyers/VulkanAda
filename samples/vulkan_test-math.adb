--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2020 Zane Myers
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Vulkan.Math;
use Vulkan.Math;
use Ada.Text_IO;

procedure Vulkan_Test.Math is

    -- This error describes a failure of a test.
    TEST_SCALAR_BOOL_FAIL : exception;

    -- Banners
    BANNER_L1 : String := "***************************************";
    BANNER_L2 : String := "---------------------------------------";

    --------------------------------------------------------------------------------
    -- @brief
    -- This operations tests the "and","or","xor","not", "=", and "/=" operators for the
    -- following Vkm types:
    --     Vkm_Bool
    --     Vkm_Bvec2
    --     Vkm_Bvec3
    --     Vkm_Bvec4
    -- The Image operation is used to display results.
    --------------------------------------------------------------------------------
    procedure Vulkan_Test_Bool is

        passed : Vkm_Bool := False;
        test_scalar_1, test_scalar_2, test_scalar_3 : Vkm_Bool := False;
    begin

        ------------------------------------------------------------------------
        -- First, various scalar boolean operations are tested.
        ------------------------------------------------------------------------
        Put_Line(Banner_L1);
        Put_Line("Testing Scalar Vkm_Bool:");
        Put_Line(Banner_L2);

        ------------------------------------------------------------------------
        -- Test the unary boolean -, +, and not operations.
        ------------------------------------------------------------------------
        Put_Line("    test_scalar_1 = " & test_scalar_1'Image);

        test_scalar_1 := not test_scalar_1;

        Put_Line("    not test_scalar_1 = " & test_scalar_1'Image);
        if test_scalar_1 = False then
            raise TEST_SCALAR_BOOL_FAIL with
                "'not' of Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_1 := False;
        test_scalar_1 := - test_scalar_1;

        Put_Line("    - test_scalar_1 = " & test_scalar_1'Image);
        if test_scalar_1 = False then
            raise TEST_SCALAR_BOOL_FAIL with
                "'-' of Vkm_Bool yielded unexpected result";
        end if;

        Put_Line("    test_scalar_1 = " & test_scalar_1'Image);

        test_scalar_1 := + test_scalar_1;

        Put_Line("    + test_scalar_1 = " & test_scalar_1'Image);

        if test_scalar_1 = False then
            raise TEST_SCALAR_BOOL_FAIL with
                "'+' of Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_1 := - test_scalar_1;

        Put_Line("    - test_scalar_1 = " & test_scalar_1'Image);

        if test_scalar_1 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'-' of Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_1 := + test_scalar_1;

        Put_Line("    + test_scalar_1 = " & test_scalar_1'Image);

        if test_scalar_1 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'+' of Vkm_Bool yielded unexpected result";
        end if;

        Put_Line(Banner_L2);

        ------------------------------------------------------------------------
        -- Test the 'and' and '*' boolean operations.  Boolean multiplication is
        -- the same as performing a boolean 'and'
        ------------------------------------------------------------------------
        -- Part 1: 0 'and' 0 = 0
        --         0   '*' 0 = 0
        ------------------------------------------------------------------------
        test_scalar_1 := False;
        test_scalar_2 := False;
        test_scalar_3 := True;
        Put_Line("    test_scalar_1= " & test_scalar_1'Image);
        Put_Line("    test_scalar_2= " & test_scalar_2'Image);
        Put_Line("    test_scalar_3= " & test_scalar_3'Image);

        test_scalar_3 := test_scalar_1 and test_scalar_2;

        Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'and' for Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_3 := True;
        test_scalar_3 := test_scalar_1 * test_scalar_2;

        Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'*' for Vkm_Bool yielded unexpected result";
        end if;

        ------------------------------------------------------------------------
        -- Part 1: 1 'and' 0 = 0
        --         1   '*' 0 = 0
        ------------------------------------------------------------------------
        test_scalar_1 := True;
        test_scalar_2 := False;
        test_scalar_3 := True;
        Put_Line("    test_scalar_1= " & test_scalar_1'Image);
        Put_Line("    test_scalar_2= " & test_scalar_2'Image);
        Put_Line("    test_scalar_3= " & test_scalar_3'Image);

        test_scalar_3 := test_scalar_1 and test_scalar_2;

        Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'and' for Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_3 := True;
        test_scalar_3 := test_scalar_1 * test_scalar_2;

        Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'*' for Vkm_Bool yielded unexpected result";
        end if;

        ------------------------------------------------------------------------
        -- Part 1: 0 'and' 1 = 0
        --         0   '*' 1 = 0
        ------------------------------------------------------------------------
        test_scalar_1 := False;
        test_scalar_2 := True;
        test_scalar_3 := True;
        Put_Line("    test_scalar_1= " & test_scalar_1'Image);
        Put_Line("    test_scalar_2= " & test_scalar_2'Image);
        Put_Line("    test_scalar_3= " & test_scalar_3'Image);

        test_scalar_3 := test_scalar_1 and test_scalar_2;

        Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'and' for Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_3 := True;
        test_scalar_3 := test_scalar_1 * test_scalar_2;

        Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = True then
            raise TEST_SCALAR_BOOL_FAIL with
                "'*' for Vkm_Bool yielded unexpected result";
        end if;

        ------------------------------------------------------------------------
        -- Part 1: 1 'and' 1 = 0
        --         1   '*' 1 = 0
        ------------------------------------------------------------------------
        test_scalar_1 := True;
        test_scalar_2 := True;
        test_scalar_3 := False;
        Put_Line("    test_scalar_1= " & test_scalar_1'Image);
        Put_Line("    test_scalar_2= " & test_scalar_2'Image);
        Put_Line("    test_scalar_3= " & test_scalar_3'Image);

        test_scalar_3 := test_scalar_1 and test_scalar_2;

        Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = False then
            raise TEST_SCALAR_BOOL_FAIL with
                "'and' for Vkm_Bool yielded unexpected result";
        end if;

        test_scalar_3 := False;
        test_scalar_3 := test_scalar_1 * test_scalar_2;

        Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);

        if test_scalar_3 = False then
            raise TEST_SCALAR_BOOL_FAIL with
                "'*' for Vkm_Bool yielded unexpected result";
        end if;

    end Vulkan_Test_Bool;

begin

    Vulkan_Test_Bool;

end Vulkan_Test.Math;
