GNATdoc.Documentation = {
  "label": "Vulkan.Math.GenIType",
  "qualifier": "",
  "summary": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "This package describes any length vector of Vkm_Int type.\n"
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
          "text": "Provides an instantiation of the generic GenType  package with a Base_Type of \n"
        },
        {
          "kind": "span",
          "text": "Vkm_Int. This is used to provide the Vkm_GenIType subtype for the Vulkan Math \n"
        },
        {
          "kind": "span",
          "text": "library.\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Vkm_GenIType",
          "qualifier": "",
          "line": 51,
          "column": 13,
          "src": "srcs/vulkan-math-genitype.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 51,
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
                      "text": "Vkm_GenIType",
                      "href": "docs/vulkan__math__genitype___spec.html#L51C13"
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
                      "text": "GIT.Vkm_GenType",
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
                  "text": "A subtype of the instantiated Vkm_GenType that represents the GenIType \n"
                },
                {
                  "kind": "span",
                  "text": "described in the GLSL specification.\n"
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
          "label": "Apply_Func_IVI_IVI_IVB_RVI",
          "qualifier": "",
          "line": 83,
          "column": 14,
          "src": "srcs/vulkan-math-genitype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function for parameters of Vkm_Int and Vkm_Bool type to GenIType\n"
                },
                {
                  "kind": "span",
                  "text": "and GenBType vectors.\n"
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
                  "number": 83,
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
                      "text": "Apply_Func_IVI_IVI_IVB_RVI",
                      "href": "docs/vulkan__math__genitype___spec.html#L83C14"
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
                      "href": "docs/vulkan__math__genitype___spec.html#L83C41"
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
                      "text": "IVI2",
                      "href": "docs/vulkan__math__genitype___spec.html#L83C47"
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
                      "href": "docs/vulkan__math__genitype___spec.html#L51C13"
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
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "IVB1",
                      "href": "docs/vulkan__math__genitype___spec.html#L84C41"
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
                      "text": "Vkm_GenIType",
                      "href": "docs/vulkan__math__genitype___spec.html#L51C13"
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
                  "text": "Applies a supplied function component wise on two GenIType vectors and\n"
                },
                {
                  "kind": "span",
                  "text": "a GenBType vector returning a GenIType vector.\n"
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
                      "text": "RVI := [Func(IVI1.x, IVI2.x, IVB1.x) ... Func(IVI1.w, IVI2.w, IVB1.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IVI1",
              "line": 83,
              "column": 41,
              "type": {
                "label": "Vulkan.Math.GenIType.Vkm_GenIType",
                "docHref": "docs/vulkan__math__genitype___spec.html#L51C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input GenIType parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IVI2",
              "line": 83,
              "column": 47,
              "type": {
                "label": "Vulkan.Math.GenIType.Vkm_GenIType",
                "docHref": "docs/vulkan__math__genitype___spec.html#L51C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input GenIType parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IVB1",
              "line": 84,
              "column": 41,
              "type": {
                "label": "Vulkan.Math.GenBType.Vkm_GenBType",
                "docHref": "docs/vulkan__math__genbtype___spec.html#L48C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input GenBType parameter.\n"
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
                    "text": "The result GenIType vector, RVI.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Apply_Func_IVI_IVI_RVB",
          "qualifier": "",
          "line": 109,
          "column": 14,
          "src": "srcs/vulkan-math-genitype.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Apply function on two Vkm_Int inputs that returns a Vkm_Bool component-wise\n"
                },
                {
                  "kind": "span",
                  "text": "to two Vkm_GenIType vectors.\n"
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
                  "number": 109,
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
                      "text": "Apply_Func_IVI_IVI_RVB",
                      "href": "docs/vulkan__math__genitype___spec.html#L109C14"
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
                      "href": "docs/vulkan__math__genitype___spec.html#L109C37"
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
                      "text": "IVI2",
                      "href": "docs/vulkan__math__genitype___spec.html#L109C43"
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
                      "href": "docs/vulkan__math__genitype___spec.html#L51C13"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Applies a supplied function component wise on two GenIType vectors, \n"
                },
                {
                  "kind": "span",
                  "text": "returning a GenBType vector.\n"
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
                      "text": "RVB := [Func(IVI1.x,IVI2.x) ... Func(IVI1.w,IVI2.w)]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "IVI1",
              "line": 109,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.GenIType.Vkm_GenIType",
                "docHref": "docs/vulkan__math__genitype___spec.html#L51C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first input GenIType parameter.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "IVI2",
              "line": 109,
              "column": 43,
              "type": {
                "label": "Vulkan.Math.GenIType.Vkm_GenIType",
                "docHref": "docs/vulkan__math__genitype___spec.html#L51C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second input GenIType parameter.\n"
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
                    "text": "The resulting GenBType vector, RVB.\n"
                  }
                ]
              }
            ]
          }
        }
      ],
      "label": "Subprograms"
    }
  ]
};