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
package body Vulkan.Math.Vec2 is

-- Access components, Getters
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2i)  return Basic_Type is
    
        component : Basic_Type;
        
    begin
        
        case comp is
        
            when 0 => 
                component := this.comp_1;
                
            when 1 =>
                component := this.comp_2;
                
        end case;
        
        return component;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
    
    function Get (This : in     Vkm_Vec2;
                  comp : in     Vkm_Vec2xy) return Basic_Type is
    
        component : Basic_Type;
        
    begin
        
        case comp is
        
            when x => 
                component := this.comp_1;
                
            when y =>
                component := this.comp_2;
                
        end case;
        
        return component;
    end Get;      
    
    
    ----------------------------------------------------------------------------
    
    
    function Get (This : in     Vkm_Vec2;
                  comp : in     Vkm_Vec2rg) return Basic_Type is
    
        component : Basic_Type;
        
    begin
        
        case comp is
        
            when r => 
                component := this.comp_1;
                
            when g =>
                component := this.comp_2;
                
        end case;
        
        return component;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
    
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2st) return Basic_Type is
    
        component : Basic_Type;
        
    begin
        
        case comp is
        
            when s => 
                component := this.comp_1;
                
            when t =>
                component := this.comp_2;
                
        end case;
        
        return component;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
    
    function Get (This : in     Vkm_Vec2;    
                  comp : in     Vkm_Vec2xy_Swizzle2) return Vkm_Vec2 is 
                  
        swiz : Vkm_Vec2 := Make;
    begin
        
        case comp is
        
            when xx =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_1;
                
            when xy =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_2;
                
            when yx =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_1;
                
            when yy =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_2;
        end case;
        
        return swiz;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
    
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2rg_Swizzle2) return Vkm_Vec2 is 
                  
        swiz : Vkm_Vec2 := Make;
    begin
        
        case comp is
        
            when rr =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_1;
                
            when rg =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_2;
                
            when gr =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_1;
                
            when gg =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_2;
        end case;
        
        return swiz;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
                     
    function Get (This : in     Vkm_Vec2; 
                  comp : in     Vkm_Vec2st_Swizzle2) return Vkm_Vec2 is 
                  
        swiz : Vkm_Vec2 := Make;
    begin
        
        case comp is
        
            when ss =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_1;
                
            when st =>
                swiz.comp_1 := This.comp_1;
                swiz.comp_2 := This.comp_2;
                
            when ts =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_1;
                
            when tt =>
                swiz.comp_1 := This.comp_2;
                swiz.comp_2 := This.comp_2;
        end case;
        
        return swiz;
    end Get;
    
    
    ----------------------------------------------------------------------------
    
    
    -- Access components, Setters
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2i; 
                   value : in     Basic_Type) is
    
    begin
        
        case comp is
        
            when 0 => 
                this.comp_1 := value;
                
            when 1 =>
                this.comp_2 := value;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
                      
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2xy; 
                   value : in     Basic_Type) is
    
    begin
        
        case comp is
        
            when x => 
                this.comp_1 := value;
                
            when y =>
                this.comp_2 := value;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
       
    procedure Set (This  : in out Vkm_Vec2;
                   comp  : in     Vkm_Vec2rg; 
                   value : in     Basic_Type) is
    
    begin
        
        case comp is
        
            when r => 
                this.comp_1 := value;
                
            when g =>
                this.comp_2 := value;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
                       
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2st; 
                   value : in     Basic_Type) is
    
    begin
        
        case comp is
        
            when s => 
                this.comp_1 := value;
                
            when t =>
                this.comp_2 := value;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
     
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2xy_Swizzle2; 
                   value : in     Vkm_Vec2) is
    
    begin
        
        case comp is
        
            when xx => 
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_1;
                
            when xy =>
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_2;
                
            when yx =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_1;
               
            when yy =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_2;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
             
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2rg_Swizzle2; 
                   Value : in     Vkm_Vec2) is
    
    begin
        
        case comp is
        
            when rr => 
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_1;
                
            when rg =>
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_2;
                
            when gr =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_1;
               
            when gg =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_2;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
    
                    
    procedure Set (This  : in out Vkm_Vec2; 
                   comp  : in     Vkm_Vec2st_Swizzle2; 
                   value : in     Vkm_Vec2) is
    
    begin
        
        case comp is
        
            when ss => 
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_1;
                
            when st =>
                this.comp_1 := value.comp_1;
                this.comp_2 := value.comp_2;
                
            when ts =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_1;
               
            when tt =>
                this.comp_1 := value.comp_2;
                this.comp_2 := value.comp_2;
                
        end case;
        
    end Set;
    
    
    ----------------------------------------------------------------------------
     
    
    -- Get the length of the vector
    function Length(This : in    Vkm_Vec2) return Vkm_Vec2_Size is 
        pragma unreferenced (This);
    begin
        
        return Vkm_Vec2_Size(2);
    end Length;
    
    
    ----------------------------------------------------------------------------
    
        
    function To_String(This : in    Vkm_Vec2) return String is 
                  
    begin
        
        return " [ " & Image(This.comp_1) & 
               " , " & Image(This.comp_2) & " ] ";
    end To_String;
    
    
    ----------------------------------------------------------------------------
    
        
    -- Constructors
    function Make return Vkm_Vec2 is 
                  
        -- Instance is initialized with the default values of the Basic_Type.
        instance : constant Vkm_Vec2 := (others => <>);
    begin
        
        return instance;
    end Make;
    
    
    ----------------------------------------------------------------------------
    
     
    function Make(value : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        instance : constant Vkm_Vec2 :=
            (comp_1 => value.comp_1,
             comp_2 => value.comp_2);
    begin
    
        return instance;
        
    end Make;
    
    
    ----------------------------------------------------------------------------
    
     
    -- Explicit Basic Constructors
    function Make(scalar : in     Basic_Type) return Vkm_Vec2 is 
                  
        instance : constant Vkm_Vec2 := 
            (comp_1 => scalar,
             comp_2 => scalar);            
    begin
    
        return instance;
        
    end Make;
    
    
    ----------------------------------------------------------------------------
    
         
    function Make(x : in     Basic_Type;
                  y : in     Basic_Type) return Vkm_Vec2 is 
                  
        instance : constant Vkm_Vec2 :=
            (comp_1 => x,
             comp_2 => y);
    begin
        
        return instance;
    end Make;
    
    
    ----------------------------------------------------------------------------
                  
                  
    ----------------------------------------------------------------------------
    -- Override Standard Operators
    ----------------------------------------------------------------------------
    -- Unary Operators
    function "+"  (Right : in Vkm_Vec2) return Vkm_Vec2 is 
                 
        Result : constant Vkm_Vec2 := (Make(Right));
    begin
        return Result;
    end  "+";
    
    
    ----------------------------------------------------------------------------
    
          
    function "-"  (Right : in Vkm_Vec2) return Vkm_Vec2 is 
    
        Result : constant Vkm_Vec2 := 
            (comp_1 => -Right.comp_1,
             comp_2 => -Right.comp_2);
    begin
        return Result;
    end "-";
    
    
    ----------------------------------------------------------------------------
    
       
    function "not" (Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => not Right.comp_1,
             comp_2 => not Right.comp_2);
    begin
        return Result;
    end "not";
    
    
    ----------------------------------------------------------------------------
    
    
    function "abs" (Right : in    Vkm_Vec2) return Vkm_Vec2 is
    
        Result : constant Vkm_Vec2 :=
            (comp_1 => abs Right.comp_1,
             comp_2 => abs Right.comp_2);
             
    begin
        return Result;
    end "abs";
    
    
    ----------------------------------------------------------------------------
   
    
    -- Binary Operators
    function "+"  (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 := 
            (comp_1 => Left.comp_1 + Right.comp_1,
             comp_2 => Left.comp_2 + Right.comp_2);
            
    begin
        return Result;
    end "+";
    
    
    ----------------------------------------------------------------------------
    
         
    function "+"  (Left  : in     Vkm_Vec2;
                   Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Left.comp_2 + Right);
    begin
        return Result;
    end "+";
    
    
    ----------------------------------------------------------------------------
    
                    
    function "+"  (Left  : in     Basic_Type;
                   Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left + Right.comp_1,
             comp_2 => Left + Right.comp_2);
                  
    begin
        return Result;
    end "+";
    
    
    ----------------------------------------------------------------------------
    
       
    function "-"  (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 - Right.comp_1,
             comp_2 => Left.comp_2 - Right.comp_2);
                  
    begin
        return Result;
    end "-";
    
    
    ----------------------------------------------------------------------------
    
       
    function "-"  (Left  : in     Vkm_Vec2;
                   Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 - Right,
             comp_2 => Left.comp_2 - Right);
                  
    begin
        return Result;
    end "-";
    
    
    ----------------------------------------------------------------------------
    
       
    function "-"  (Left  : in     Basic_Type;
                   Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left - Right.comp_1,
             comp_2 => Left - Right.comp_2);
                  
    begin
        return Result;
    end "-";
    
    
    ----------------------------------------------------------------------------
    
       
    function "*"  (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 * Right.comp_1,
             comp_2 => Left.comp_2 * Right.comp_2);
                  
    begin
        return Result;
    end "*";
    
    
    ----------------------------------------------------------------------------
    
       
    function "*"  (Left  : in     Vkm_Vec2;
                   Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 * Right,
             comp_2 => Left.comp_2 * Right);
                  
    begin
        return Result;
    end "*";
    
    
    ----------------------------------------------------------------------------
    
       
    function "*"  (Left  : in     Basic_Type;
                   Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left * Right.comp_1,
             comp_2 => Left * Right.comp_2);
                  
    begin
        return Result;
    end "*";
    
    
    ----------------------------------------------------------------------------
    
       
    function "/"  (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 / Right.comp_1,
             comp_2 => Left.comp_2 / Right.comp_2);
                  
    begin
        return Result;
    end "/";
    
    
    ----------------------------------------------------------------------------
    
       
    function "/"  (Left  : in     Vkm_Vec2;
                   Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 / Right,
             comp_2 => Left.comp_2 / Right);
                  
    begin
        return Result;
    end "/";
    
    
    ----------------------------------------------------------------------------
    
       
    function "/"  (Left  : in     Basic_Type;
                   Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left / Right.comp_1,
             comp_2 => Left / Right.comp_2);
                  
    begin
        null;
    end "/";
    
    
    ----------------------------------------------------------------------------
    
       
    function "mod" (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "mod";
    
    
    ----------------------------------------------------------------------------
    
       
    function "mod" (Left  : in     Vkm_Vec2;
                    Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "mod";
    
    
    ----------------------------------------------------------------------------
    
       
    function "mod" (Left  : in     Basic_Type;
                    Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "mod";
    
    
    ----------------------------------------------------------------------------
    
       
    function "rem" (Left, Right : in Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "rem";
    
    
    ----------------------------------------------------------------------------
    
       
    function "rem" (Left  : in     Vkm_Vec2;
                    Right : in     Basic_Type) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "rem";
    
    
    ----------------------------------------------------------------------------
    
       
    function "rem" (Left  : in     Basic_Type;
                    Right : in     Vkm_Vec2) return Vkm_Vec2 is 
                  
        Result : constant Vkm_Vec2 :=
            (comp_1 => Left.comp_1 + Right,
             comp_2 => Right.comp_2 + Right);
                  
    begin
        null;
    end "rem";
    
    
    ----------------------------------------------------------------------------
    
                    
    -- Relational Operators
    function "/=" (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
                  
    begin
        null;
    end "/=";
    
    
    ----------------------------------------------------------------------------
    
         
    function "="  (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "=";
    
    
    ----------------------------------------------------------------------------
    
           
    function ">"  (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end ">";
    
    
    ----------------------------------------------------------------------------
    
          
    function ">=" (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end ">=";
    
    
    ----------------------------------------------------------------------------
    
          
    function "<"  (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "<";
    
    
    ----------------------------------------------------------------------------
    
          
    function "<=" (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "<=";
    
    
    ----------------------------------------------------------------------------
    
         
    
    -- Logical Operators
    function "and" (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "and";
    
    
    ----------------------------------------------------------------------------
    
          
    function "or"  (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "or";
    
    
    ----------------------------------------------------------------------------
    
          
    function "xor" (Left, Right : in Vkm_Vec2) return Vkm_Bool is 
                  
    begin
        null;
    end "xor";
    
       
end Vulkan.Math.Vec2;
