GNATdoc.Documentation = {
  "label": "Vulkan.Math.GenType",
  "qualifier": "",
  "summary": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "This generic package provides constructors, getters and setters, swizzlers,\n"
        },
        {
          "kind": "span",
          "text": "and functors for a generic vector type.\n"
        }
      ]
    }
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Vkm_Vector",
          "qualifier": "",
          "line": 46,
          "column": 10,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_Vector",
                      "href": "docs/vulkan__math__gentype___spec.html#L46C10"
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
                      "text": "array"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_Indices",
                      "href": "docs/vulkan__math___spec.html#L70C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "range"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "<>"
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
                      "text": "of"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "aliased"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base_Type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The vector type is an array with indices in the range of the Vkm_Indices\n"
                },
                {
                  "kind": "span",
                  "text": "type [0 .. 3]. Because of this, the length of any vector instance is \n"
                },
                {
                  "kind": "span",
                  "text": "in the range [1 .. 4].\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Simple types"
    },
    {
      "entities": [
        {
          "label": "Vkm_GenType_Reference",
          "qualifier": "",
          "line": 68,
          "column": 10,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 68,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_GenType_Reference",
                      "href": "docs/vulkan__math__gentype___spec.html#L68C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector"
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
                      "text": "not"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "null"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "null"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "record"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 69,
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
                      "text": " Implicit_Dereference => vector"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "A reference to a generic vector type. The Vkm_GenType instance is \n"
                },
                {
                  "kind": "span",
                  "text": "automatically dereferenced on use.\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Record types"
    },
    {
      "entities": [
        {
          "label": "Vkm_GenType",
          "qualifier": "",
          "line": 61,
          "column": 10,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "last_index",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C22"
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
                      "cssClass": "identifier",
                      "text": "Vkm_Indices",
                      "href": "docs/vulkan__math___spec.html#L70C10"
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
                      "text": "tagged"
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
                      "text": "record"
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
                      "text": "            "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "data",
                      "href": "docs/vulkan__math__gentype___spec.html#L63C13"
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
                      "cssClass": "identifier",
                      "text": "Vkm_Vector",
                      "href": "docs/vulkan__math__gentype___spec.html#L46C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vkm_Indices",
                      "href": "docs/vulkan__math___spec.html#L70C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "'"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "First"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ".."
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "last_index"
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
                  "number": 64,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
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
                      "cssClass": "keyword",
                      "text": "record"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The gentype is a discriminant tagged record which encapsulates the \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_Vector type. This allows use of \".\" to perform functions on an \n"
                },
                {
                  "kind": "span",
                  "text": "instance of vector. \n"
                }
              ]
            }
          ],
          "inherited": [
            {
              "label": "Vkm_GenBType",
              "docHref": "docs/vulkan__math__genbtype___spec.html#L35C13"
            },
            {
              "label": "Vkm_GenIType",
              "docHref": "docs/vulkan__math__genitype___spec.html#L38C13"
            },
            {
              "label": "Vkm_GenDType",
              "docHref": "docs/vulkan__math__gendtype___spec.html#L40C13"
            },
            {
              "label": "Vkm_GenUType",
              "docHref": "docs/vulkan__math__genutype___spec.html#L37C13"
            },
            {
              "label": "Vkm_GenFType",
              "docHref": "docs/vulkan__math__genftype___spec.html#L42C13"
            }
          ],
          "fields": [
            {
              "label": "last_index",
              "line": 61,
              "column": 22,
              "type": {
                "label": "Vulkan.Math.Vkm_Indices",
                "docHref": "docs/vulkan__math___spec.html#L70C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The discriminant, last_index, determines the size of Vkm_GenType's\n"
                    },
                    {
                      "kind": "span",
                      "text": "vector.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "data",
              "line": 63,
              "column": 13,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_Vector",
                "docHref": "docs/vulkan__math__gentype___spec.html#L46C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The vector data for the record. This information is able to be\n"
                    },
                    {
                      "kind": "span",
                      "text": "passed to a C/C++ context.\n"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      "label": "Tagged types"
    },
    {
      "entities": [
        {
          "label": "Apply_Func_IS_IV_RV",
          "qualifier": "",
          "line": 9834,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9834,
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
                      "text": "Apply_Func_IS_IV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9834C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/vulkan__math__gentype___spec.html#L9834C34"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
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
                      "text": "Base_Type"
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
                  "number": 9835,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right",
                      "href": "docs/vulkan__math__gentype___spec.html#L9835C39"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 9834,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 9835,
              "column": 39,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_IS_IS_RV",
          "qualifier": "",
          "line": 9857,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9857,
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
                      "text": "Apply_Func_IV_IS_IS_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9857C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9857C37"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 9858,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IS1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9858C37"
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
                      "text": "IS2",
                      "href": "docs/vulkan__math__gentype___spec.html#L9858C42"
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
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 9857,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "IS1",
              "line": 9858,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
              ]
            },
            {
              "label": "IS2",
              "line": 9858,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_IS_RV",
          "qualifier": "",
          "line": 9838,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9838,
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
                      "text": "Apply_Func_IV_IS_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9838C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/vulkan__math__gentype___spec.html#L9838C34"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 9839,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right",
                      "href": "docs/vulkan__math__gentype___spec.html#L9839C34"
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
                      "text": "Base_Type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 9838,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 9839,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_IV_IS_RV",
          "qualifier": "",
          "line": 9853,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9853,
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
                      "text": "Apply_Func_IV_IV_IS_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9853C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9853C37"
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
                      "text": "IV2",
                      "href": "docs/vulkan__math__gentype___spec.html#L9853C42"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 9854,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IS1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9854C37"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
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
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 9853,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "IV2",
              "line": 9853,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "IS1",
              "line": 9854,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_IV_IV_RV",
          "qualifier": "",
          "line": 9850,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9850,
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
                      "text": "Apply_Func_IV_IV_IV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9850C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9850C37"
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
                      "text": "IV2",
                      "href": "docs/vulkan__math__gentype___spec.html#L9850C42"
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
                      "text": "IV3",
                      "href": "docs/vulkan__math__gentype___spec.html#L9850C47"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 9850,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "IV2",
              "line": 9850,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "IV3",
              "line": 9850,
              "column": 47,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_IV_RV",
          "qualifier": "",
          "line": 9831,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9831,
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
                      "text": "Apply_Func_IV_IV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9831C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left",
                      "href": "docs/vulkan__math__gentype___spec.html#L9831C34"
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
                      "text": "Right",
                      "href": "docs/vulkan__math__gentype___spec.html#L9831C40"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Left",
              "line": 9831,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "Right",
              "line": 9831,
              "column": 40,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_OV_RV",
          "qualifier": "",
          "line": 9846,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9846,
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
                      "text": "Apply_Func_IV_OV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9846C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9846C34"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 9847,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "OV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L9847C34"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 9846,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            },
            {
              "label": "OV1",
              "line": 9847,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Apply_Func_IV_RV",
          "qualifier": "",
          "line": 9842,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 9842,
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
                      "text": "Apply_Func_IV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L9842C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "A",
                      "href": "docs/vulkan__math__gentype___spec.html#L9842C31"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "A",
              "line": 9842,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    },
    {
      "entities": [
        {
          "label": "Concatenate",
          "qualifier": "",
          "line": 286,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Concatenate two vectors together.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 286,
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
                      "text": "Concatenate",
                      "href": "docs/vulkan__math__gentype___spec.html#L286C14"
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
                      "text": "left",
                      "href": "docs/vulkan__math__gentype___spec.html#L286C27"
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
                      "text": "right",
                      "href": "docs/vulkan__math__gentype___spec.html#L286C33"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Creates a new instance of Vkm_GenType which contains components from two\n"
                },
                {
                  "kind": "span",
                  "text": "existing instances, left and right. If left and right are both 2 dimmensional\n"
                },
                {
                  "kind": "span",
                  "text": "vectors, the new vector is a 4 dimmensional vector described as follows:\n"
                }
              ]
            },
            {
              "kind": "code",
              "children": [
                {
                  "number": 1,
                  "children": [
                    {
                      "kind": "span",
                      "text": "cat := [ left.x, left.y, right.x , right.y]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "left",
              "line": 286,
              "column": 27,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType that is on the left hand side of the concatenation.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "right",
              "line": 286,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType that is on the right hand side of the concatenation.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "The concatenated vectors.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Copy",
          "qualifier": "",
          "line": 260,
          "column": 15,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Copy components from source to target.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 260,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Copy",
                      "href": "docs/vulkan__math__gentype___spec.html#L260C15"
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
                      "text": "destination",
                      "href": "docs/vulkan__math__gentype___spec.html#L260C21"
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
                      "text": " "
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 261,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "source",
                      "href": "docs/vulkan__math__gentype___spec.html#L261C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                  "number": 262,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "num_copy",
                      "href": "docs/vulkan__math__gentype___spec.html#L262C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "    "
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
                      "text": "Vkm_Length",
                      "href": "docs/vulkan__math___spec.html#L67C10"
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
                  "number": 263,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "offset",
                      "href": "docs/vulkan__math__gentype___spec.html#L263C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
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
                      "text": "Vkm_Indices",
                      "href": "docs/vulkan__math___spec.html#L70C10"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Copies num_copy components from the source Vkm_GenType instance to the\n"
                },
                {
                  "kind": "span",
                  "text": "indicated offset of the destination Vkm_GenType instance.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "destination",
              "line": 260,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType that component values are copied into.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "source",
              "line": 261,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType that component values are copied from.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "num_copy",
              "line": 262,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.Vkm_Length",
                "docHref": "docs/vulkan__math___spec.html#L67C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The number of components to copy from the source instance.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "offset",
              "line": 263,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.Vkm_Indices",
                "docHref": "docs/vulkan__math___spec.html#L70C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The offset into the destination into the destination instance at which\n"
                    },
                    {
                      "kind": "span",
                      "text": "components should be copied into.\n"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "label": "Image",
          "qualifier": "",
          "line": 237,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The image of the vector.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 237,
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
                      "text": "Image",
                      "href": "docs/vulkan__math__gentype___spec.html#L237C14"
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
                      "text": "instance",
                      "href": "docs/vulkan__math__gentype___spec.html#L237C21"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Generates a human readable string which contains the contents of the \n"
                },
                {
                  "kind": "span",
                  "text": "instance of Vkm_GenType. The formatted string appears as follows for\n"
                },
                {
                  "kind": "span",
                  "text": "a vector with three components:\n"
                }
              ]
            },
            {
              "kind": "code",
              "children": [
                {
                  "number": 1,
                  "children": [
                    {
                      "kind": "span",
                      "text": "\"[ \" & Image(vec.x) & \", \" & Image(vec.y) & \", \" & Image(vec.z) & \" ]\""
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": ""
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The Image() subprogram that is supplied during generic instantiation is used\n"
                },
                {
                  "kind": "span",
                  "text": "to print the component of Base_Type.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "instance",
              "line": 237,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "The human readable image of the vector.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Length",
          "qualifier": "",
          "line": 214,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The number of components in the instance of Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 214,
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
                      "text": "Length",
                      "href": "docs/vulkan__math__gentype___spec.html#L214C14"
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
                      "text": "instance",
                      "href": "docs/vulkan__math__gentype___spec.html#L214C22"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_Length",
                      "href": "docs/vulkan__math___spec.html#L67C10"
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
                  "number": 215,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "instance.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "'"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Length"
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
                      "text": "with"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " Inline"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The number of components in the instance of Vkm_GenType.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "instance",
              "line": 214,
              "column": 22,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "The dimmension of the vector.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 94,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 94,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L94C14"
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
                      "text": "last_index",
                      "href": "docs/vulkan__math__gentype___spec.html#L94C20"
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
                      "text": "Vkm_Indices",
                      "href": "docs/vulkan__math___spec.html#L70C10"
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
                  "number": 95,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "value",
                      "href": "docs/vulkan__math__gentype___spec.html#L95C20"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
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
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType with size indicated by\n"
                },
                {
                  "kind": "span",
                  "text": "Last_Index, and all components set to a specified value.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "last_index",
              "line": 94,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.Vkm_Indices",
                "docHref": "docs/vulkan__math___spec.html#L70C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The last index of the vector. Components can be can be accessed using \n"
                    },
                    {
                      "kind": "span",
                      "text": "indices [0 .. last_index].\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value",
              "line": 95,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that each component of the instance Vkm_GenType should be set to.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 112,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 112,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L112C14"
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
                      "text": "value1",
                      "href": "docs/vulkan__math__gentype___spec.html#L112C20"
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
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType with one component set\n"
                },
                {
                  "kind": "span",
                  "text": "to the indicated value.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value1",
              "line": 112,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the first component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 132,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 132,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L132C14"
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
                      "text": "value1",
                      "href": "docs/vulkan__math__gentype___spec.html#L132C20"
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
                      "text": "value2",
                      "href": "docs/vulkan__math__gentype___spec.html#L132C28"
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
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType with two components set\n"
                },
                {
                  "kind": "span",
                  "text": "to the indicated values.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value1",
              "line": 132,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the first component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value2",
              "line": 132,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the second component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 155,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 155,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L155C14"
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
                      "text": "value1",
                      "href": "docs/vulkan__math__gentype___spec.html#L155C20"
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
                      "text": "value2",
                      "href": "docs/vulkan__math__gentype___spec.html#L155C28"
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
                      "text": "value3",
                      "href": "docs/vulkan__math__gentype___spec.html#L155C36"
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
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType with three components set\n"
                },
                {
                  "kind": "span",
                  "text": "to the indicated values.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value1",
              "line": 155,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the first component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value2",
              "line": 155,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the second component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value3",
              "line": 155,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the third component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 181,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 181,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L181C14"
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
                      "text": "value1",
                      "href": "docs/vulkan__math__gentype___spec.html#L181C20"
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
                      "text": "value2",
                      "href": "docs/vulkan__math__gentype___spec.html#L181C28"
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
                      "text": "value3",
                      "href": "docs/vulkan__math__gentype___spec.html#L181C36"
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
                      "text": "value4",
                      "href": "docs/vulkan__math__gentype___spec.html#L181C44"
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
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base_Type"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType with four components set\n"
                },
                {
                  "kind": "span",
                  "text": "to the indicated values.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value1",
              "line": 181,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the first component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value2",
              "line": 181,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the second component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value3",
              "line": 181,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the third component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value4",
              "line": 181,
              "column": 44,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L34C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that the fourth component of the Vkm_GenType vector is set to.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make",
          "qualifier": "",
          "line": 199,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor of type Vkm_GenType.\n"
                }
              ]
            }
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 199,
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
                      "text": "Make",
                      "href": "docs/vulkan__math__gentype___spec.html#L199C14"
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
                      "text": "value",
                      "href": "docs/vulkan__math__gentype___spec.html#L199C20"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
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
                      "text": "Vkm_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L61C10"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation creates an instance of Vkm_GenType by copying an existing\n"
                },
                {
                  "kind": "span",
                  "text": "instance of Vkm_GenType. The new instance is of the same dimmension and\n"
                },
                {
                  "kind": "span",
                  "text": "value as the instance used for construction.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 199,
              "column": 20,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The instance of Vkm_GenType used to initialize the new instance of Vkm_GenType.\n"
                    }
                  ]
                }
              ]
            }
          ],
          "returns": {
            "description": [
              {
                "kind": "paragraph",
                "children": [
                  {
                    "kind": "span",
                    "text": "A new instance of Vkm_GenType.\n"
                  }
                ]
              }
            ]
          }
        }
      ],
      "label": "Dispatching subprograms"
    }
  ]
};