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
          "line": 77,
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
                  "number": 77,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L77C10"
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
          "line": 99,
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
                  "number": 99,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L99C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 100,
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
          "line": 92,
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
                  "number": 92,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C22"
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
                  "number": 93,
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
                  "number": 94,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L94C13"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L77C10"
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
                  "number": 95,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "docHref": "docs/vulkan__math__genitype___spec.html#L51C13"
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
              "docHref": "docs/vulkan__math__genftype___spec.html#L55C13"
            }
          ],
          "fields": [
            {
              "label": "last_index",
              "line": 92,
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
              "line": 94,
              "column": 13,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_Vector",
                "docHref": "docs/vulkan__math__gentype___spec.html#L77C10"
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
          "line": 10112,
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
                  "number": 10112,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10112C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10112C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10112C42"
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
                  "number": 10113,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10113C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10112,
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
              "line": 10112,
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
              "line": 10113,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 9911,
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
                  "number": 9911,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9911C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9911C34"
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
                  "number": 9912,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9912C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9911,
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
              "line": 9912,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 10084,
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
                  "number": 10084,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10084C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10084C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 10085,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10085C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10085C42"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10084,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10085,
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
              "line": 10085,
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
          "line": 9935,
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
                  "number": 9935,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9935C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9935C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 9936,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9936C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9935,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9936,
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
          "line": 10056,
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
                  "number": 10056,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10056C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10056C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10056C42"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 10057,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10057C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10056,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10056,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10057,
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
          "line": 10029,
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
                  "number": 10029,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10029C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10029C37"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10029C42"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10029C47"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10029,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10029,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10029,
              "column": 47,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 9887,
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
                  "number": 9887,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9887C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9887C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9887C40"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9887,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9887,
              "column": 40,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 10002,
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
                  "number": 10002,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10002C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10002C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 10003,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L10003C34"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10002,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 10003,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 9956,
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
                  "number": 9956,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9956C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L9956C31"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 9956,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 317,
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
                  "number": 317,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L317C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L317C27"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L317C33"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 317,
              "column": 27,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 317,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 291,
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
                  "number": 291,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L291C15"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L291C21"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "text": "source",
                      "href": "docs/vulkan__math__gentype___spec.html#L292C21"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "text": "num_copy",
                      "href": "docs/vulkan__math__gentype___spec.html#L293C21"
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
                  "number": 294,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L294C21"
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
              "line": 291,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 292,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 293,
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
              "line": 294,
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
          "line": 268,
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
                  "number": 268,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L268C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L268C21"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 268,
              "column": 21,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 245,
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
                  "number": 245,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L245C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L245C22"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                  "number": 246,
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
              "line": 245,
              "column": 22,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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
          "line": 125,
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
                  "number": 125,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L125C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L125C28"
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
                  "number": 126,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L126C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 125,
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
              "line": 126,
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
          "line": 143,
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
                  "number": 143,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L143C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L143C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 143,
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
          "line": 163,
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
                  "number": 163,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L163C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L163C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L163C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 163,
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
              "line": 163,
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
          "line": 186,
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
                  "number": 186,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L186C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L186C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L186C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L186C44"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 186,
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
              "line": 186,
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
              "line": 186,
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
          "line": 212,
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
                  "number": 212,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L212C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L212C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L212C36"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L212C44"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L212C52"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 212,
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
              "line": 212,
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
              "line": 212,
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
              "line": 212,
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
          "line": 230,
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
                  "number": 230,
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
                      "href": "docs/vulkan__math__gentype___spec.html#L230C14"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L230C28"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
                      "href": "docs/vulkan__math__gentype___spec.html#L92C10"
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
              "line": 230,
              "column": 28,
              "type": {
                "label": "Vulkan.Math.GenType.Vkm_GenType",
                "docHref": "docs/vulkan__math__gentype___spec.html#L92C10"
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