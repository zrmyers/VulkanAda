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
generic

    -- The type of component that makes up the vector. This can be either a 
    -- floating point or a discrete type.
    type Basic_Type is private;
    with function Image (This        : in     Basic_Type) return String;
    with function "-"   (Right       : in     Basic_Type) return Basic_Type;
    with function "not" (Right       : in     Basic_Type) return Basic_Type;
    with function "+"   (Left, Right : in     Basic_Type) return Basic_Type;
    with function "-"   (Left, Right : in     Basic_Type) return Basic_Type;
    with function "abs" (Right       : in     Basic_Type) return Basic_Type;
    with function "*"   (Left, Right : in     Basic_Type) return Basic_Type;
package Vulkan.Math.Vec2 is
    pragma Preelaborate;
    
    -- Vkm_Vec2 Size 
    type Vkm_Vec2_Size is new Integer range 0 .. 2;
    
    -- Useful access Types
    subtype Vkm_Vec2i is Vkm_Vec2_Size range 0 .. 1;
    type Vkm_Vec2xy is ( x, y );    
    type Vkm_Vec2rg is ( r, g );
    type Vkm_Vec2st is ( s, t );
    
    -- Useful Swizzle access types.
    type Vkm_Vec2xy_Swizzle2 is ( xx, xy, yx, yy);
    type Vkm_Vec2rg_Swizzle2 is ( rr, rg, gr, gg);
    type Vkm_Vec2st_Swizzle2 is ( ss, st, ts, tt);
    
    -- Data definition.
    type Vkm_Vec2 is tagged record
        comp_1, comp_2 : Basic_Type;
    end record;
    pragma Convention (Convention => C,
                       Entity     => Vkm_Vec2);
                      
    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------
    -- Access components, Getters
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2i)  return Basic_Type;                  
    function Get (This : in     Vkm_Vec2;
                  comp : in     Vkm_Vec2xy) return Basic_Type;                  
    function Get (This : in     Vkm_Vec2;
                  comp : in     Vkm_Vec2rg) return Basic_Type;                  
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2st) return Basic_Type;                  
    function Get (This : in     Vkm_Vec2;    
                  comp : in     Vkm_Vec2xy_Swizzle2) return Vkm_Vec2;                  
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2rg_Swizzle2) return Vkm_Vec2;                  
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2st_Swizzle2) return Vkm_Vec2;
    
    -- Access components, Setters
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2i; 
                   value : in     Basic_Type);                   
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2xy; 
                   value : in     Basic_Type);    
    procedure Set (This  : in out Vkm_Vec2;
                   comp  : in     Vkm_Vec2rg; 
                   value : in     Basic_Type);                   
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2st; 
                   value : in     Basic_Type);
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2xy_Swizzle2; 
                   value : in     Vkm_Vec2);                   
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2rg_Swizzle2; 
                   Value : in     Vkm_Vec2);                   
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2st_Swizzle2; 
                   value : in     Vkm_Vec2);
    
    -- Get the length of the vector
    function Length(This : in    Vkm_Vec2) return Vkm_Vec2_Size;
    pragma Warnings (Off, "foreign convention function ""To_String"" should not return unconstrained array");
        
    function To_String(This : in    Vkm_Vec2) return String;    
    pragma Warnings (On, "foreign convention function ""To_String"" should not return unconstrained array");
        
    -- Constructors
    function Make return Vkm_Vec2;
    function Make(value : in     Vkm_Vec2) return Vkm_Vec2;
    
    -- Explicit Basic Constructors
    function Make(scalar : in     Basic_Type) return Vkm_Vec2;    
    function Make(x : in     Basic_Type;
                  y : in     Basic_Type) return Vkm_Vec2;
                  
    ----------------------------------------------------------------------------
    -- Basic Vector Operations
    ----------------------------------------------------------------------------
    
    ----------------------------------------------------------------------------
    -- Override Standard Operators
    ----------------------------------------------------------------------------
    -- Unary Operators
    function "+"  (Right : in     Vkm_Vec2) return Vkm_Vec2;    
    function "-"  (Right : in     Vkm_Vec2) return Vkm_Vec2;
    function "not" (Right : in    Vkm_Vec2) return Vkm_Vec2;
    function "abs" (Right : in    Vkm_Vec2) return Vkm_Vec2;
    
    -- Binary Operators
    function "+"   (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;    
    function "+"   (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;                   
    function "+"   (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "-"   (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "-"   (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "-"   (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "*"   (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "*"   (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "*"   (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "/"   (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "/"   (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "/"   (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "mod" (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "mod" (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "mod" (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "rem" (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "rem" (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "rem" (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
    function "**"  (Left, Right : in Vkm_Vec2)       return Vkm_Vec2;
    function "**"  (Left        : in     Vkm_Vec2;
                    Right       : in     Basic_Type) return Vkm_Vec2;
    function "**"  (Left        : in     Basic_Type;
                    Right       : in     Vkm_Vec2)   return Vkm_Vec2;
                    
    -- Relational Operators
    function "/=" (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function "="  (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function ">"  (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function ">=" (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function "<"  (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function "<=" (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    
    -- Logical Operators
    function "and" (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function "or"  (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
    function "xor" (Left, Right : in Vkm_Vec2) return Vkm_Bool;    
        
end Vulkan.Math.Vec2;
