GNATdoc.SourceFile = {
  "kind": "code",
  "children": [
    {
      "kind": "line",
      "number": 1,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--------------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 2,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- MIT License"
        }
      ]
    },
    {
      "kind": "line",
      "number": 3,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 4,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Copyright (c) 2020 Zane Myers"
        }
      ]
    },
    {
      "kind": "line",
      "number": 5,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 6,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Permission is hereby granted, free of charge, to any person obtaining a copy"
        }
      ]
    },
    {
      "kind": "line",
      "number": 7,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- of this software and associated documentation files (the \"Software\"), to deal"
        }
      ]
    },
    {
      "kind": "line",
      "number": 8,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- in the Software without restriction, including without limitation the rights"
        }
      ]
    },
    {
      "kind": "line",
      "number": 9,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell"
        }
      ]
    },
    {
      "kind": "line",
      "number": 10,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- copies of the Software, and to permit persons to whom the Software is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 11,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- furnished to do so, subject to the following conditions:"
        }
      ]
    },
    {
      "kind": "line",
      "number": 12,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 13,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- The above copyright notice and this permission notice shall be included in all"
        }
      ]
    },
    {
      "kind": "line",
      "number": 14,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- copies or substantial portions of the Software."
        }
      ]
    },
    {
      "kind": "line",
      "number": 15,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 16,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
        }
      ]
    },
    {
      "kind": "line",
      "number": 17,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
        }
      ]
    },
    {
      "kind": "line",
      "number": 18,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
        }
      ]
    },
    {
      "kind": "line",
      "number": 19,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
        }
      ]
    },
    {
      "kind": "line",
      "number": 20,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
        }
      ]
    },
    {
      "kind": "line",
      "number": 21,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
        }
      ]
    },
    {
      "kind": "line",
      "number": 22,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- SOFTWARE."
        }
      ]
    },
    {
      "kind": "line",
      "number": 23,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--------------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 24,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- This package describes a generic Single Precision Floating Point Vulkan Math type."
        }
      ]
    },
    {
      "kind": "line",
      "number": 25,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--------------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 26,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 27,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenBType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 28,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenIType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 29,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenUType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 30,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 31,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "use"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenBType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 32,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "use"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenUType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 33,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "use"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenIType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 34,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 35,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "package"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L35C21"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 36,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "pragma"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Preelaborate"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 37,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "pragma"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Pure"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 38,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 39,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "package"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "GFT"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        }
      ]
    },
    {
      "kind": "line",
      "number": 40,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Base_Type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "=>"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Image"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "=>"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "'"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Image"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 41,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 42,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "subtype"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "GFT.Vkm_GenType",
          "href": "docs/vulkan__math__gentype___spec.html#L91C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 43,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 44,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "----------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 45,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "-- Generic Operations"
        }
      ]
    },
    {
      "kind": "line",
      "number": 46,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "----------------------------------------------------------------------------"
        }
      ]
    },
    {
      "kind": "line",
      "number": 47,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 48,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L48C23"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF1",
          "href": "docs/vulkan__math__genftype___spec.html#L48C28"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF2",
          "href": "docs/vulkan__math__genftype___spec.html#L48C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 49,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                           "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISB1",
          "href": "docs/vulkan__math__genftype___spec.html#L49C28"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Bool",
          "href": "docs/vulkan__math___spec.html#L49C10"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 50,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_IVF_IVB_RVF",
          "href": "docs/vulkan__math__genftype___spec.html#L50C14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF1",
          "href": "docs/vulkan__math__genftype___spec.html#L50C41"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF2",
          "href": "docs/vulkan__math__genftype___spec.html#L50C47"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 51,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                                        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVB1",
          "href": "docs/vulkan__math__genftype___spec.html#L51C41"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenBType",
          "href": "docs/vulkan__math__genbtype___spec.html#L48C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 52,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 53,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 54,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L54C23"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF",
          "href": "docs/vulkan__math__genftype___spec.html#L54C28"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Bool",
          "href": "docs/vulkan__math___spec.html#L49C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 55,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_RVB",
          "href": "docs/vulkan__math__genftype___spec.html#L55C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF1",
          "href": "docs/vulkan__math__genftype___spec.html#L55C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenBType",
          "href": "docs/vulkan__math__genbtype___spec.html#L48C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 56,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 57,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 58,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L58C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF",
          "href": "docs/vulkan__math__genftype___spec.html#L58C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Int",
          "href": "docs/vulkan__math___spec.html#L58C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 59,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_RVI",
          "href": "docs/vulkan__math__genftype___spec.html#L59C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF1",
          "href": "docs/vulkan__math__genftype___spec.html#L59C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenIType",
          "href": "docs/vulkan__math__genitype___spec.html#L38C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 60,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 61,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 62,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L62C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISI",
          "href": "docs/vulkan__math__genftype___spec.html#L62C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Int",
          "href": "docs/vulkan__math___spec.html#L58C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 63,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVI_RVF",
          "href": "docs/vulkan__math__genftype___spec.html#L63C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVI1",
          "href": "docs/vulkan__math__genftype___spec.html#L63C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenIType",
          "href": "docs/vulkan__math__genitype___spec.html#L38C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 64,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 65,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 66,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L66C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF",
          "href": "docs/vulkan__math__genftype___spec.html#L66C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Uint",
          "href": "docs/vulkan__math___spec.html#L55C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 67,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_RVU",
          "href": "docs/vulkan__math__genftype___spec.html#L67C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF1",
          "href": "docs/vulkan__math__genftype___spec.html#L67C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenUType",
          "href": "docs/vulkan__math__genutype___spec.html#L50C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 68,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 69,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 70,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L70C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISU",
          "href": "docs/vulkan__math__genftype___spec.html#L70C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Uint",
          "href": "docs/vulkan__math___spec.html#L55C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 71,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVU_RVF",
          "href": "docs/vulkan__math__genftype___spec.html#L71C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVU1",
          "href": "docs/vulkan__math__genftype___spec.html#L71C34"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenUType",
          "href": "docs/vulkan__math__genutype___spec.html#L50C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 72,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 73,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 74,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L74C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF",
          "href": "docs/vulkan__math__genftype___spec.html#L74C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 75,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                            "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "OSI",
          "href": "docs/vulkan__math__genftype___spec.html#L75C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "out"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Int",
          "href": "docs/vulkan__math___spec.html#L58C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 76,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_OVI_RVF",
          "href": "docs/vulkan__math__genftype___spec.html#L76C14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF",
          "href": "docs/vulkan__math__genftype___spec.html#L76C37"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 77,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                                    "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "OVI",
          "href": "docs/vulkan__math__genftype___spec.html#L77C37"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "out"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenIType",
          "href": "docs/vulkan__math__genitype___spec.html#L38C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 78,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 79,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 80,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L80C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF",
          "href": "docs/vulkan__math__genftype___spec.html#L80C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 81,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                            "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISI",
          "href": "docs/vulkan__math__genftype___spec.html#L81C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Int",
          "href": "docs/vulkan__math___spec.html#L58C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 82,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_IVI_RVF",
          "href": "docs/vulkan__math__genftype___spec.html#L82C14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF",
          "href": "docs/vulkan__math__genftype___spec.html#L82C37"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 83,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                                    "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVI",
          "href": "docs/vulkan__math__genftype___spec.html#L83C37"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenIType",
          "href": "docs/vulkan__math__genitype___spec.html#L38C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 84,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 85,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "generic"
        }
      ]
    },
    {
      "kind": "line",
      "number": 86,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Func",
          "href": "docs/vulkan__math__genftype___spec.html#L86C23"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF1",
          "href": "docs/vulkan__math__genftype___spec.html#L86C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 87,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                            "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "ISF2",
          "href": "docs/vulkan__math__genftype___spec.html#L87C29"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Float",
          "href": "docs/vulkan__math___spec.html#L61C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_Bool",
          "href": "docs/vulkan__math___spec.html#L49C10"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 88,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "function"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Apply_Func_IVF_IVF_RVB",
          "href": "docs/vulkan__math__genftype___spec.html#L88C14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF1",
          "href": "docs/vulkan__math__genftype___spec.html#L88C37"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ","
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "IVF2",
          "href": "docs/vulkan__math__genftype___spec.html#L88C43"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "in"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenFType",
          "href": "docs/vulkan__math__genftype___spec.html#L42C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "return"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vkm_GenBType",
          "href": "docs/vulkan__math__genbtype___spec.html#L48C13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 89,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 90,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "end"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Vulkan.Math.GenFType"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    }
  ],
  "label": "vulkan-math-genftype.ads"
};