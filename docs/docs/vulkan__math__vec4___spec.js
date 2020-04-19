GNATdoc.Documentation = {
  "label": "Vulkan.Math.Vec4",
  "qualifier": "",
  "summary": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "This package provides a single precision floating point vector type with \n"
        },
        {
          "kind": "span",
          "text": "4 components.\n"
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
          "label": "Vkm_Vec4",
          "qualifier": "",
          "line": 44,
          "column": 13,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                      "text": "Vkm_GenFType",
                      "href": "docs/vulkan__math__genftype___spec.html#L55C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Last_Index"
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
                      "cssClass": "number",
                      "text": "3"
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
                  "text": "A 4-component single precision floating point vector.\n"
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
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 62,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 62,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L62C14"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 63,
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
                      "text": "GFT.Make_GenType"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Last_Index"
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
                      "cssClass": "number",
                      "text": "3"
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
                      "text": "value"
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
                      "cssClass": "number",
                      "text": "0.0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a default vector with all components set to 0.0.\n"
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
                    "text": "A Vec4 with all components set to 0.0.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 79,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L79C14"
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
                      "text": "scalar_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L79C25"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 80,
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
                      "text": "GFT.Make_GenType"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Last_Index"
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
                      "cssClass": "number",
                      "text": "3"
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
                      "text": "value"
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
                      "text": "scalar_value"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector with all components set to the same value.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "scalar_value",
              "line": 79,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value to set all components to.\n"
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
                    "text": "A Vec4 with all components set to scalar_value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 97,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 97,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L97C14"
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
                      "text": "vec4_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L97C25"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 98,
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
                      "text": "GFT.Make_GenType"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec4_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec4_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
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
                      "text": "                          "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec4_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec4_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "3"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by copying components from an existing vector.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vec4_value",
              "line": 97,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec4.Vkm_Vec4",
                "docHref": "docs/vulkan__math__vec4___spec.html#L44C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The vec4 to copy components from.\n"
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
                    "text": "A vec4 with all of its components set equal to the corresponding\n"
                  },
                  {
                    "kind": "span",
                    "text": "components of vec4_value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "(renaming)",
          "line": 124,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L124C14"
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
                      "href": "docs/vulkan__math__vec4___spec.html#L124C25"
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
                      "href": "docs/vulkan__math__vec4___spec.html#L124C33"
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
                      "href": "docs/vulkan__math__vec4___spec.html#L124C41"
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
                      "href": "docs/vulkan__math__vec4___spec.html#L124C49"
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
                      "text": "    "
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "renames"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "GFT.Make_GenType"
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
                  "text": "Produce a vector by specifying the values for each of its components.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value1",
              "line": 124,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Value for component 1.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value2",
              "line": 124,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Value for component 2.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value3",
              "line": 124,
              "column": 41,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Value for componetn 3.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "value4",
              "line": 124,
              "column": 49,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Value for component 4.\n"
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
                    "text": "A Vec4 with all components set as specified.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 146,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 146,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L146C14"
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
                      "text": "scalar_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L146C25"
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
                  "number": 147,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L147C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Vkm_Vec3",
                      "href": "docs/vulkan__math__vec3___spec.html#L42C13"
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
                      "text": "Vkm_Vec3",
                      "href": "docs/vulkan__math__vec3___spec.html#L42C13"
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
                  "number": 148,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar_value"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 149,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 150,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 151,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating a scalar value with a vec3.\n"
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
                      "text": "vec4 = [scalar_value, vec3_value  ]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "scalar_value",
              "line": 146,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The scalar value to concatenate with the vec3.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "vec3_value",
              "line": 147,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec3.Vkm_Vec3",
                "docHref": "docs/vulkan__math__vec3___spec.html#L42C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The vec3 to concatenate to the scalar value.\n"
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
                    "text": "The instance of Vec4.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 172,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 172,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L172C14"
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
                      "text": "vec3_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L172C25"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
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
                      "text": "Vkm_Vec3",
                      "href": "docs/vulkan__math__vec3___spec.html#L42C13"
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
                  "number": 173,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L173C25"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 174,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 175,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 176,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec3_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 177,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar_value"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating a vec3 with a scalar value.\n"
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
                      "text": "vec4 = [vec3_value, scalar_value]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vec3_value",
              "line": 172,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec3.Vkm_Vec3",
                "docHref": "docs/vulkan__math__vec3___spec.html#L42C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The vec3 to concatenate to the scalar value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "scalar_value",
              "line": 173,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The scalar value to concatenate to the vec3.\n"
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
                    "text": "The instance of Vec4.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 198,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 198,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L198C14"
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
                      "text": "vec2_value1",
                      "href": "docs/vulkan__math__vec4___spec.html#L198C25"
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
                      "text": "vec2_value2",
                      "href": "docs/vulkan__math__vec4___spec.html#L198C38"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 199,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value1.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value1.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 200,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value2.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value2.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating a vec2 with a vec2.\n"
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
                      "text": "vec4 = [vec2_value1, vec2_value2]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vec2_value1",
              "line": 198,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec2.Vkm_Vec2",
                "docHref": "docs/vulkan__math__vec2___spec.html#L39C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The first vec2.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "vec2_value2",
              "line": 198,
              "column": 38,
              "type": {
                "label": "Vulkan.Math.Vec2.Vkm_Vec2",
                "docHref": "docs/vulkan__math__vec2___spec.html#L39C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The second vec2.\n"
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
                    "text": "The instance of Vec4.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 224,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 224,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L224C14"
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
                      "text": "scalar1",
                      "href": "docs/vulkan__math__vec4___spec.html#L224C25"
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
                      "text": "scalar2",
                      "href": "docs/vulkan__math__vec4___spec.html#L224C34"
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
                  "number": 225,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L225C25"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 226,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar1"
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
                      "text": "scalar2"
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
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating two scalar values with a vec2.\n"
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
                      "text": "vec4 = [scalar1, scalar2, vec2_value]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "scalar1",
              "line": 224,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "First scalar value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "scalar2",
              "line": 224,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Second scalar value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "vec2_value",
              "line": 225,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec2.Vkm_Vec2",
                "docHref": "docs/vulkan__math__vec2___spec.html#L39C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The Vec2 value.\n"
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
                    "text": "The instance of Vec4.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 250,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 250,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L250C14"
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
                      "text": "scalar1",
                      "href": "docs/vulkan__math__vec4___spec.html#L250C25"
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
                  "number": 251,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L251C25"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                  "number": 252,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar2",
                      "href": "docs/vulkan__math__vec4___spec.html#L252C25"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 253,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar1"
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
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                      "text": "scalar2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating two scalar values with a vec2.\n"
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
                      "text": "vec4 = [scalar1, vec2_value, scalar2]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "scalar1",
              "line": 250,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "First scalar value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "vec2_value",
              "line": 251,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec2.Vkm_Vec2",
                "docHref": "docs/vulkan__math__vec2___spec.html#L39C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The Vec2 value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "scalar2",
              "line": 252,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Second scalar value.\n"
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
                    "text": "The instance of Vec4.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Make_Vec4",
          "qualifier": "",
          "line": 277,
          "column": 14,
          "src": "srcs/vulkan-math-vec4.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Constructor for Vkm_Vec4 type.\n"
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
                  "number": 277,
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
                      "text": "Make_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L277C14"
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
                      "text": "vec2_value",
                      "href": "docs/vulkan__math__vec4___spec.html#L277C25"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                  "number": 278,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar1",
                      "href": "docs/vulkan__math__vec4___spec.html#L278C25"
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
                  "number": 279,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "scalar2",
                      "href": "docs/vulkan__math__vec4___spec.html#L279C25"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "number": 280,
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
                      "text": "Make_Vec4"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "0"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vec2_value.data"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "number",
                      "text": "1"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                      "text": "scalar1"
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
                      "text": "scalar2"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
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
                  "text": "Produce a vector by concatenating two scalar values with a vec2.\n"
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
                      "text": "vec4 = [vec2_value, scalar1, scalar2]"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vec2_value",
              "line": 277,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vec2.Vkm_Vec2",
                "docHref": "docs/vulkan__math__vec2___spec.html#L39C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The Vec2 value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "scalar1",
              "line": 278,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "First scalar value.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "scalar2",
              "line": 279,
              "column": 25,
              "type": {
                "label": "Vulkan.Math.Vkm_Float",
                "docHref": "docs/vulkan__math___spec.html#L61C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "Second scalar value.\n"
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
                    "text": "The instance of Vec4.\n"
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