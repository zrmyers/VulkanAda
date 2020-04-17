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
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "The Vkm_GenType is a tagged record which contains either a 1D, 2D, 3D, or 4D\n"
        },
        {
          "kind": "span",
          "text": "vector of elements of the generic Base_Type.\n"
        }
      ]
    },
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "The components of each vector can be accessed via component names XYZW, RGBA,\n"
        },
        {
          "kind": "span",
          "text": "or STPQ.  Additionally, components of a generic type can be set with these\n"
        },
        {
          "kind": "span",
          "text": "names. The following is an example of getting the second component of one 4D\n"
        },
        {
          "kind": "span",
          "text": "vector and setting a component of another 4D vector:\n"
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
              "text": "declare"
            }
          ]
        },
        {
          "number": 2,
          "children": [
            {
              "kind": "span",
              "text": "    vec1, vec2 : Vkm_GenType(last_index => 3);"
            }
          ]
        },
        {
          "number": 3,
          "children": [
            {
              "kind": "span",
              "text": "begin"
            }
          ]
        },
        {
          "number": 4,
          "children": [
            {
              "kind": "span",
              "text": "    -- Set vec1.x to vec2.y"
            }
          ]
        },
        {
          "number": 5,
          "children": [
            {
              "kind": "span",
              "text": "    vec1.x(vec2.y)"
            }
          ]
        },
        {
          "number": 6,
          "children": [
            {
              "kind": "span",
              "text": "end"
            }
          ]
        },
        {
          "number": 7,
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
          "text": "Swizzling can also be done to obtain different permutations of a components \n"
        },
        {
          "kind": "span",
          "text": "from vectors or set different combinations of components in a vector.\n"
        },
        {
          "kind": "span",
          "text": "An example is shown below:\n"
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
              "text": "declare"
            }
          ]
        },
        {
          "number": 2,
          "children": [
            {
              "kind": "span",
              "text": "    vec1, vec2 : Vkm_GenType(last_index => 3);"
            }
          ]
        },
        {
          "number": 3,
          "children": [
            {
              "kind": "span",
              "text": "begin"
            }
          ]
        },
        {
          "number": 4,
          "children": [
            {
              "kind": "span",
              "text": "    -- Set vec1.x to vec2.y and vec1.w to vec2.z."
            }
          ]
        },
        {
          "number": 5,
          "children": [
            {
              "kind": "span",
              "text": "    vec1.xw(vec2.yz)"
            }
          ]
        },
        {
          "number": 6,
          "children": [
            {
              "kind": "span",
              "text": "end"
            }
          ]
        },
        {
          "number": 7,
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
          "text": "The following restrictions apply to swizzling:\n"
        },
        {
          "kind": "ul",
          "children": [
            {
              "kind": "li",
              "children": [
                {
                  "kind": "span",
                  "text": "A swizzle get or set operation will raise a constraint exception if attempting to\n"
                },
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "access a component only available for vectors of dimmension greater than\n"
                    },
                    {
                      "kind": "span",
                      "text": "the dimmension of the vector the get operation is being performed on.\n"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "li",
              "children": [
                {
                  "kind": "span",
                  "text": "A swizzle set cannot be done for combinations that include multiple of the\n"
                },
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "same component. I.e. vec1.xx(vec2.xy) is not allowed.\n"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Vkm_Vector",
          "qualifier": "",
          "line": 76,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L76C10"
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
          "line": 98,
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
                  "number": 98,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L98C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "number": 99,
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
          "line": 91,
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
                  "number": 91,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C22"
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
                  "number": 92,
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
                  "number": 93,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L93C13"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L76C10"
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
                  "number": 94,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "docHref": "docs/vulkan__math__genbtype___spec.html#L48C13"
            },
            {
              "label": "Vkm_GenIType",
              "docHref": "docs/vulkan__math__genitype___spec.html#L38C13"
            },
            {
              "label": "Vkm_GenDType",
              "docHref": "docs/vulkan__math__gendtype___spec.html#L53C13"
            },
            {
              "label": "Vkm_GenUType",
              "docHref": "docs/vulkan__math__genutype___spec.html#L50C13"
            },
            {
              "label": "Vkm_GenFType",
              "docHref": "docs/vulkan__math__genftype___spec.html#L42C13"
            }
          ],
          "fields": [
            {
              "label": "last_index",
              "line": 91,
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
              "line": 93,
              "column": 13,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_Vector",
                "docHref": "docs/vulkan__math__gentype___spec.html#L76C10"
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
          "label": "Apply_Func_IS_IS_IV_RV",
          "qualifier": "",
          "line": 10089,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 10089,
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
                      "text": "Apply_Func_IS_IS_IV_RV",
                      "href": "docs/vulkan__math__gentype___spec.html#L10089C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IS1",
                      "href": "docs/vulkan__math__gentype___spec.html#L10089C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10089C42"
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
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 10090,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IV1",
                      "href": "docs/vulkan__math__gentype___spec.html#L10090C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on one input vectors and two\n"
                },
                {
                  "kind": "span",
                  "text": "scalar values.\n"
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
                      "text": "RV := [Func(IS1, IS2, IV1.x) ... Func(IS1, IS2, IV1.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IS1",
              "line": 10089,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input scalar parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IS2",
              "line": 10089,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input scalar parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IV1",
              "line": 10090,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IS_IV_RV",
          "qualifier": "",
          "line": 9888,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply binary function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 9888,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9888C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9888C34"
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
                  "number": 9889,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "right",
                      "href": "docs/vulkan__math__gentype___spec.html#L9889C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise between a vector and a scalar.\n"
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
                      "text": "RV := [Func(IS,IV2.x) ... Func(IS, IV2.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "left",
              "line": 9888,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input scalar parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "right",
              "line": 9889,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_IS_IS_RV",
          "qualifier": "",
          "line": 10061,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 10061,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10061C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10061C37"
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
                  "number": 10062,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10062C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10062C42"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on one input vectors and two\n"
                },
                {
                  "kind": "span",
                  "text": "scalar values.\n"
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
                      "text": "RV := [Func(IV1.x, IS1, IS2) ... Func(IV1.w, IS1, IS2)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 10061,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IS1",
              "line": 10062,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input scalar parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IS2",
              "line": 10062,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input scalar parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_IS_RV",
          "qualifier": "",
          "line": 9912,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply binary function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 9912,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9912C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9912C34"
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
                  "number": 9913,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "right",
                      "href": "docs/vulkan__math__gentype___spec.html#L9913C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise between a vector and a scalar.\n"
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
                      "text": "RV := [Func(IV1.x,IS) ... Func(IV1.w, IS)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "left",
              "line": 9912,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input scalar parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "right",
              "line": 9913,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_IV_IS_RV",
          "qualifier": "",
          "line": 10033,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 10033,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10033C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10033C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10033C42"
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
                  "number": 10034,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10034C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on two input vectors and a\n"
                },
                {
                  "kind": "span",
                  "text": "scalar value.\n"
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
                      "text": "RV := [Func(IV1.x, IV2.x, IS1) ... Func(IV1.w, IV2.w, IS1)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 10033,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IV2",
              "line": 10033,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IS1",
              "line": 10034,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first scalar parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_IV_IV_RV",
          "qualifier": "",
          "line": 10006,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 10006,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10006C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10006C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10006C42"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10006C47"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on three input vectors.\n"
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
                      "text": "RV := [Func(IV1.x, IV2.x, IV3.x) ... Func(IV1.w, IV2.w, IV3.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 10006,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IV2",
              "line": 10006,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IV3",
              "line": 10006,
              "column": 47,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The third input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_IV_RV",
          "qualifier": "",
          "line": 9864,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply binary function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 9864,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9864C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9864C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9864C40"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise between two vectors.\n"
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
                      "text": "RV := [Func(IV1.x,IV2.x) ... Func(IV1.w, IV2.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "left",
              "line": 9864,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "right",
              "line": 9864,
              "column": 40,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_OV_RV",
          "qualifier": "",
          "line": 9979,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 9979,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9979C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9979C34"
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
                  "number": 9980,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9980C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on a vector. This outputs a\n"
                },
                {
                  "kind": "span",
                  "text": "vector parameter in addition to the return value.\n"
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
                      "text": "RV := [Func(IV1.x, OV1.x) ... Func(IV1.w, OV1.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IV1",
              "line": 9979,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The input vector parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "OV1",
              "line": 9980,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The output vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IV_RV",
          "qualifier": "",
          "line": 9933,
          "column": 14,
          "src": "srcs/vulkan-math-gentype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply unary function for parameters of Base_Type on Vkm_GenType vector.\n"
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
                  "number": 9933,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9933C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9933C31"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "text": "Apply's a supplied function component wise on a vector.\n"
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
                      "text": "RV := [Func(A.x) ... Func(A.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "A",
              "line": 9933,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The input vector parameter.\n"
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
                    "text": "The result, RV.\n"
                  }
                ]
              }
            ]
          }
        }
      ],
      "label": "Subprograms"
    },
    {
      "entities": [
        {
          "label": "Concatenate",
          "qualifier": "",
          "line": 316,
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
                  "number": 316,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L316C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L316C27"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L316C33"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 316,
              "column": 27,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 316,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
          "line": 290,
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
                  "number": 290,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L290C15"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L290C21"
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
                  "number": 291,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L291C21"
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
                  "number": 292,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L292C21"
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
                  "number": 293,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L293C21"
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
              "line": 290,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 291,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 292,
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
              "line": 293,
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
          "line": 267,
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
                  "number": 267,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L267C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L267C21"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 267,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
          "line": 244,
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
                  "number": 244,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L244C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L244C22"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                  "number": 245,
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
              "line": 244,
              "column": 22,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 124,
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
                  "number": 124,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L124C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L124C28"
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
                  "number": 125,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                           "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "value",
                      "href": "docs/vulkan__math__gentype___spec.html#L125C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 124,
              "column": 28,
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
              "line": 125,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 142,
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
                  "number": 142,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L142C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L142C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 142,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 162,
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
                  "number": 162,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L162C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L162C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L162C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 162,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 162,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 185,
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
                  "number": 185,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L185C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L185C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L185C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L185C44"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 185,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 185,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 185,
              "column": 44,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 211,
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
                  "number": 211,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L211C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L211C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L211C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L211C44"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L211C52"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 211,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 211,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 211,
              "column": 44,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
              "line": 211,
              "column": 52,
              "type": {
                "label": "Vulkan.Math.GenType.Base_Type",
                "docHref": "docs/vulkan__math__gentype___spec.html#L64C10"
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
          "label": "Make_GenType",
          "qualifier": "",
          "line": 229,
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
                  "number": 229,
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
                      "text": "Make_GenType",
                      "href": "docs/vulkan__math__gentype___spec.html#L229C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L229C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L91C10"
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
              "line": 229,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L91C10"
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