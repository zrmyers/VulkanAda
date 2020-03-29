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
--  with Vulkan.Math.Bool;
--  use Vulkan.Math.Bool;
--  with Vulkan.Math.Bvec2;
--  use Vulkan.Math.Bvec2;
--  use Ada.Text_IO;
--  with Vulkan.Math.Int;
--  use Vulkan.Math.Int;

--use type Vulkan.Math.Bvec2.Vkm_Bvec2;

procedure Vulkan_Test.Math is

    -- This error describes a failure of a test.
    TEST_SCALAR_BOOL_FAIL : exception;
    TEST_BVEC2_FAIL : exception;
    
    -- Banners
    BANNER_L1 : String := "***************************************";
    BANNER_L2 : String := "---------------------------------------";

    --------------------------------------------------------------------------------
    -- @brief
    -- This operations tests the "and"(*),"or" (+),"not" (-), "xor" (/)
    --
    -- The Image operation is used to display results.
    --------------------------------------------------------------------------------
        procedure Vulkan_Test_Bool is
--  
--          test_scalar_1, test_scalar_2, test_scalar_3 : Vkm_Bool := False;
        begin
            NULL;
        ------------------------------------------------------------------------
        -- First, various scalar boolean operations are tested.
        ------------------------------------------------------------------------
--          Put_Line(Banner_L1);
--          Put_Line("Testing Scalar Vkm_Bool Operations");
--          Put_Line(Banner_L2);

        ------------------------------------------------------------------------
        -- Test the unary boolean -, +, and not operations.
        ------------------------------------------------------------------------
--          Put_Line("    test_scalar_1 = " & test_scalar_1'Image);
--  
--          test_scalar_1 := not test_scalar_1;
--  
--          Put_Line("    not test_scalar_1 = " & test_scalar_1'Image);
--          if test_scalar_1 = False then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'not' of Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_1 := False;
--          test_scalar_1 := - test_scalar_1;
--  
--          Put_Line("    - test_scalar_1 = " & test_scalar_1'Image);
--          if test_scalar_1 = False then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'-' of Vkm_Bool yielded unexpected result";
--          end if;
--  
--          Put_Line("    test_scalar_1 = " & test_scalar_1'Image);
--  
--          test_scalar_1 := + test_scalar_1;
--  
--          Put_Line("    + test_scalar_1 = " & test_scalar_1'Image);
--  
--          if test_scalar_1 = False then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'+' of Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_1 := - test_scalar_1;
--  
--          Put_Line("    - test_scalar_1 = " & test_scalar_1'Image);
--  
--          if test_scalar_1 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'-' of Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_1 := + test_scalar_1;
--  
--          Put_Line("    + test_scalar_1 = " & test_scalar_1'Image);
--  
--          if test_scalar_1 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'+' of Vkm_Bool yielded unexpected result";
--          end if;
--  
--          Put_Line(Banner_L2);

        ------------------------------------------------------------------------
        -- Test the 'and' and '*' boolean operations.  Boolean multiplication is
        -- the same as performing a boolean 'and'
        ------------------------------------------------------------------------
        -- Part 1: 0 'and' 0 = 0
        --         0   '*' 0 = 0
        ------------------------------------------------------------------------
--          test_scalar_1 := False;
--          test_scalar_2 := False;
--          test_scalar_3 := True;
--          Put_Line("    test_scalar_1= " & test_scalar_1'Image);
--          Put_Line("    test_scalar_2= " & test_scalar_2'Image);
--          Put_Line("    test_scalar_3= " & test_scalar_3'Image);
--  
--          test_scalar_3 := test_scalar_1 and test_scalar_2;
--  
--          Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'and' for Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_3 := True;
--          test_scalar_3 := test_scalar_1 * test_scalar_2;
--  
--          Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'*' for Vkm_Bool yielded unexpected result";
--          end if;

        ------------------------------------------------------------------------
        -- Part 2: 1 'and' 0 = 0
        --         1   '*' 0 = 0
        ------------------------------------------------------------------------
--          test_scalar_1 := True;
--          test_scalar_2 := False;
--          test_scalar_3 := True;
--          Put_Line("    test_scalar_1= " & test_scalar_1'Image);
--          Put_Line("    test_scalar_2= " & test_scalar_2'Image);
--          Put_Line("    test_scalar_3= " & test_scalar_3'Image);
--  
--          test_scalar_3 := test_scalar_1 and test_scalar_2;
--  
--          Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'and' for Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_3 := True;
--          test_scalar_3 := test_scalar_1 * test_scalar_2;
--  
--          Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'*' for Vkm_Bool yielded unexpected result";
--          end if;

        ------------------------------------------------------------------------
        -- Part 3: 0 'and' 1 = 0
        --         0   '*' 1 = 0
        ------------------------------------------------------------------------
--          test_scalar_1 := False;
--          test_scalar_2 := True;
--          test_scalar_3 := True;
--          Put_Line("    test_scalar_1= " & test_scalar_1'Image);
--          Put_Line("    test_scalar_2= " & test_scalar_2'Image);
--          Put_Line("    test_scalar_3= " & test_scalar_3'Image);
--  
--          test_scalar_3 := test_scalar_1 and test_scalar_2;
--  
--          Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'and' for Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_3 := True;
--          test_scalar_3 := test_scalar_1 * test_scalar_2;
--  
--          Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = True then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'*' for Vkm_Bool yielded unexpected result";
--          end if;

        ------------------------------------------------------------------------
        -- Part 4: 1 'and' 1 = 0
        --         1   '*' 1 = 0
        ------------------------------------------------------------------------
--          test_scalar_1 := True;
--          test_scalar_2 := True;
--          test_scalar_3 := False;
--          Put_Line("    test_scalar_1= " & test_scalar_1'Image);
--          Put_Line("    test_scalar_2= " & test_scalar_2'Image);
--          Put_Line("    test_scalar_3= " & test_scalar_3'Image);
--  
--          test_scalar_3 := test_scalar_1 and test_scalar_2;
--  
--          Put_Line("    test_scalar_1 and test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = False then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'and' for Vkm_Bool yielded unexpected result";
--          end if;
--  
--          test_scalar_3 := False;
--          test_scalar_3 := test_scalar_1 * test_scalar_2;
--  
--          Put_Line("    test_scalar_1 * test_scalar_2 = " & test_scalar_3'Image);
--  
--          if test_scalar_3 = False then
--              raise TEST_SCALAR_BOOL_FAIL with
--                  "'*' for Vkm_Bool yielded unexpected result";
--          end if;
--  
--          Put_Line(Banner_L2);
--  
        end Vulkan_Test_Bool;

        procedure Vulkan_Test_Bvec2 is
    
    -- Try various methods for construction a Bvec2
--      x : Vkm_Bvec2 := True & False;
--      y : Vkm_Bvec2 := Make(True);
--      z : Vkm_Bvec2 := Make;
--      w : Vkm_Bvec2 := Make(False, True);
--      
        begin
            null;
        ------------------------------------------------------------------------
        -- Testing Bvec2 operations
        ------------------------------------------------------------------------
--          Put_Line(Banner_L1);
--          Put_Line("Testing 2D Bool Vector Operations");
--          Put_Line(Banner_L2);
--          Put_Line("x         = " & x.To_String);
--          Put_Line("y         = " & y.To_String);
--          Put_Line("z         = " & z.To_String);
--          Put_Line("w         = " & w.To_String);
--          Put_Line("w.get(yx) = " & w.get(yx).To_String);
--          Put_Line("y or z    = " & To_String(y or z));
--          Put_Line("y and z   = " & To_String(y and z));
--          Put_Line("x xor y   = " & To_String(x xor y));
--          Put_Line("y.is_all = " & y.is_all'Image);
--          Put_Line("x.is_all = " & x.is_all'Image);
--          Put_Line("z.is_any = " & z.is_any'Image);
--          Put_Line("x.is_any = " & x.is_any'Image);
--          
--          if (x.get(0) /= true) or (x.get(1) /= false) then
--              raise TEST_BVEC2_FAIL with "'&' operator failed.";
--          end if;
--          
--          if w.get(yx) /= x then
--              raise TEST_BVEC2_FAIL with "'/=' operator failed.";
--          end if;
--          
--          if (x xor y) /= (False & True) then
--              raise TEST_BVEC2_FAIL with "'xor' operator failed.";
--          end if;
        
        -- Will do with manual verification for now.
        end Vulkan_Test_Bvec2;
--      
--      Procedure Vulkan_Test_Int is
--      
--          x : Vkm_Int := 0;
--          y : Vkm_Int := 23;
--          z : Vkm_Int := 4;
--          w : Vkm_Int := -42;
--          
--      begin
    
        ------------------------------------------------------------------------
        -- Testing Scalar Integer operations
        ------------------------------------------------------------------------
--          Put_Line(Banner_L1);
--          Put_Line("Testing Scalar Integer Operations");
--          Put_Line(Banner_L2);
--          
--          Put_Line("x         = " & x'Image);
--          Put_Line("y         = " & y'Image);
--          Put_Line("z         = " & z'Image);
--          Put_Line("w         = " & w'Image);
--          Put_Line("w.Sign    = " & Sign(w)'Image);
--          Put_Line("Max(w,x)  = " & Max(w,x)'Image);
--    end Vulkan_Test_Int;
begin


    -- Bool tests
    Vulkan_Test_Bool;

    Vulkan_Test_Bvec2;
    
    -- Int tests
 --   Vulkan_Test_Int;
    
end Vulkan_Test.Math;
