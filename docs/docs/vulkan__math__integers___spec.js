GNATdoc.Documentation = {
  "label": "Vulkan.Math.Integers",
  "qualifier": "",
  "summary": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "This package provides GLSL Integer Built-in functions.\n"
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
          "text": "All common functions operate component-wise.\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Bit_Count",
          "qualifier": "",
          "line": 465,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation counts the number of 1-bits in a 32-bit signed value.\n"
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
                  "number": 465,
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
                      "text": "Bit_Count",
                      "href": "docs/vulkan__math__integers___spec.html#L465C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L465C24"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Count the 1's bits of a 32-bit signed integer.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 465,
              "column": 24,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value for which 1's bits are counted.\n"
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
                    "text": "The number of 1's in value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bit_Count",
          "qualifier": "",
          "line": 493,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation counts the number of 1-bits in a 32-bit unsigned value.\n"
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
                  "number": 493,
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
                      "text": "Bit_Count",
                      "href": "docs/vulkan__math__integers___spec.html#L493C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L493C24"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Count the 1's bits of a 32-bit unsigned integer.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 493,
              "column": 24,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value for which 1's bits are counted.\n"
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
                    "text": "The number of 1's in value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Extract",
          "qualifier": "",
          "line": 224,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation extracts a bitfield from a 32-bit signed integer.\n"
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
                      "text": "Bitfield_Extract",
                      "href": "docs/vulkan__math__integers___spec.html#L224C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L224C31"
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
                      "text": "offset",
                      "href": "docs/vulkan__math__integers___spec.html#L224C38"
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
                      "text": "bits",
                      "href": "docs/vulkan__math__integers___spec.html#L224C46"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Extract bits starting from offset to the number of bits in the bitfield.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let bN = bits + offset -1.\n"
                },
                {
                  "kind": "span",
                  "text": "Let b0 = offset.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "In general, the bitfield is extracted from the value the value as follows:\n"
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
                      "text": "\\     | MSB         | Bitfield          | LSB        |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 3,
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
                  "text": "The result is formatted as follows, where N = bits -1: \n"
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
                      "text": "\\     |        MSB        |              Bitfield                | "
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bit   | 31     ... N+1    | N       N-1        ...   1      0    | "
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "value | bN'Val ... bN'Val | bN'Val (bN-1)'Val  ... b1'Val b0'Val |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 224,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value from which the bitfield is extracted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "offset",
              "line": 224,
              "column": 38,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The offset into the value from which the bitfield is extracted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "bits",
              "line": 224,
              "column": 46,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The number of bits in the bitfield.\n"
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
                    "text": "The extracted value from the bitfield. The most significant bits are set\n"
                  },
                  {
                    "kind": "span",
                    "text": "to the signed bit of the bitfield.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Extract",
          "qualifier": "",
          "line": 273,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation extracts a bitfield from a 32-bit unsigned integer.\n"
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
                  "number": 273,
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
                      "text": "Bitfield_Extract",
                      "href": "docs/vulkan__math__integers___spec.html#L273C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L273C31"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
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
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 274,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                              "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "offset",
                      "href": "docs/vulkan__math__integers___spec.html#L274C31"
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
                      "text": "bits",
                      "href": "docs/vulkan__math__integers___spec.html#L274C39"
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
                      "text": "Vkm_Uint",
                      "href": "docs/vulkan__math___spec.html#L55C10"
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
                  "text": "Extract bits starting from offset to the number of bits in the bitfield.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let bN = bits + offset -1.\n"
                },
                {
                  "kind": "span",
                  "text": "Let b0 = offset.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "In general, the bitfield is extracted from the value the value as follows:\n"
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
                      "text": "\\     | MSB         | Bitfield          | LSB        |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 3,
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
                  "text": "The result is formatted as follows, where N = bits -1: \n"
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
                      "text": "\\     |        MSB        |              Bitfield                | "
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bit   | 31     ... N+1    | N       N-1        ...   1      0    | "
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "value | 0      ... 0      | bN'Val (bN-1)'Val  ... b1'Val b0'Val |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 273,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value from which the bitfield is extracted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "offset",
              "line": 274,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The offset into the value from which the bitfield is extracted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "bits",
              "line": 274,
              "column": 39,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The number of bits in the bitfield.\n"
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
                    "text": "The extracted value from the bitfield. The most significant bits are set\n"
                  },
                  {
                    "kind": "span",
                    "text": "to 0.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Insert",
          "qualifier": "",
          "line": 321,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation inserts a bitfield into a 32-bit signed integer.\n"
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
                  "number": 321,
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
                      "text": "Bitfield_Insert",
                      "href": "docs/vulkan__math__integers___spec.html#L321C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "base",
                      "href": "docs/vulkan__math__integers___spec.html#L321C30"
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
                      "text": "insert",
                      "href": "docs/vulkan__math__integers___spec.html#L321C36"
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
                      "text": "offset",
                      "href": "docs/vulkan__math__integers___spec.html#L321C44"
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
                      "text": "bits",
                      "href": "docs/vulkan__math__integers___spec.html#L321C52"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Insert bits starting from offset to the number of bits in the bitfield.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let bN = bits + offset -1.\n"
                },
                {
                  "kind": "span",
                  "text": "Let b0 = offset.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "In general, the 'bits' least significant bits of 'insert' are copied into\n"
                },
                {
                  "kind": "span",
                  "text": "base as follows:\n"
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
                      "text": "\\     | MSB         | Bitfield          | LSB        |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "value | base        |      insert       | base       |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "base",
              "line": 321,
              "column": 30,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value into which the bitfield is inserted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "insert",
              "line": 321,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that is inserted into base.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "offset",
              "line": 321,
              "column": 44,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The offset into the base at which the bitfield is inserted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "bits",
              "line": 321,
              "column": 52,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The number of bits in the bitfield.\n"
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
                    "text": "The value of base with the bitfield inserted.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Insert",
          "qualifier": "",
          "line": 368,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation inserts a bitfield into a 32-bit unsigned integer.\n"
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
                  "number": 368,
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
                      "text": "Bitfield_Insert",
                      "href": "docs/vulkan__math__integers___spec.html#L368C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "base",
                      "href": "docs/vulkan__math__integers___spec.html#L368C30"
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
                      "text": "insert",
                      "href": "docs/vulkan__math__integers___spec.html#L368C36"
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
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 369,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                             "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "offset",
                      "href": "docs/vulkan__math__integers___spec.html#L369C30"
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
                      "text": "bits",
                      "href": "docs/vulkan__math__integers___spec.html#L369C38"
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
                      "text": "Vkm_Uint",
                      "href": "docs/vulkan__math___spec.html#L55C10"
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
                  "text": "Insert bits starting from offset to the number of bits in the bitfield.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let bN = bits + offset -1.\n"
                },
                {
                  "kind": "span",
                  "text": "Let b0 = offset.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "In general, the 'bits' least significant bits of 'insert' are copied into\n"
                },
                {
                  "kind": "span",
                  "text": "base as follows:\n"
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
                      "text": "\\     | MSB         | Bitfield          | LSB        |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "value | base        |      insert       | base       |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "base",
              "line": 368,
              "column": 30,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value into which the bitfield is inserted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "insert",
              "line": 368,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value that is inserted into base.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "offset",
              "line": 369,
              "column": 30,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The offset into the base at which the bitfield is inserted.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "bits",
              "line": 369,
              "column": 38,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The number of bits in the bitfield.\n"
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
                    "text": "The value of base with the bitfield inserted.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Reverse",
          "qualifier": "",
          "line": 403,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation reverses the bits of a 32-bit signed integer.\n"
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
                  "number": 403,
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
                      "text": "Bitfield_Reverse",
                      "href": "docs/vulkan__math__integers___spec.html#L403C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L403C31"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Reverse the bits of a 32-bit signed integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let vI be the value of bit at bit position I in the input value.\n"
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
                      "text": "bit    | 31  30  ... 1   0   |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "input  | v31 v30 ... v1  v0  |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "output | v0  v1  ... v30 v31 |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 403,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value which is to be reversed.\n"
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
                    "text": "The reversed value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Bitfield_Reverse",
          "qualifier": "",
          "line": 437,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation reverses the bits of a 32-bit unsigned integer.\n"
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
                  "number": 437,
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
                      "text": "Bitfield_Reverse",
                      "href": "docs/vulkan__math__integers___spec.html#L437C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L437C31"
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
                      "text": "Vkm_Uint",
                      "href": "docs/vulkan__math___spec.html#L55C10"
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
                  "text": "Reverse the bits of a 32-bit unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Let vI be the value of bit at bit position I in the input value.\n"
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
                      "text": "bit    | 31  30  ... 1   0   |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "input  | v31 v30 ... v1  v0  |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "output | v0  v1  ... v30 v31 |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 437,
              "column": 31,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value which is to be reversed.\n"
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
                    "text": "The reversed value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Find_Lsb",
          "qualifier": "",
          "line": 523,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the least significant 1-bit in the 32-bit signed\n"
                },
                {
                  "kind": "span",
                  "text": "integer.\n"
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
                  "number": 523,
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
                      "text": "Find_Lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L523C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L523C23"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Find the least significant bit in a 32-bit signed integer with a value of 1.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 523,
              "column": 23,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value to find the least significant 1-bit for.\n"
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
                    "text": "The bit position of the least significant 1-bit. -1 is returned if there\n"
                  },
                  {
                    "kind": "span",
                    "text": "are no 1-bits.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Find_Lsb",
          "qualifier": "",
          "line": 554,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the least significant 1-bit in the 32-bit unsigned\n"
                },
                {
                  "kind": "span",
                  "text": "integer.\n"
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
                  "number": 554,
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
                      "text": "Find_Lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L554C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L554C23"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Find the least significant bit in a 32-bit unsigned integer with a value of 1.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 554,
              "column": 23,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value to find the least significant 1-bit for.\n"
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
                    "text": "The bit position of the least significant 1-bit. -1 is returned if there\n"
                  },
                  {
                    "kind": "span",
                    "text": "are no 1-bits.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Find_Msb",
          "qualifier": "",
          "line": 585,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the most significant 0-bit in the 32-bit signed\n"
                },
                {
                  "kind": "span",
                  "text": "integer.\n"
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
                  "number": 585,
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
                      "text": "Find_Msb",
                      "href": "docs/vulkan__math__integers___spec.html#L585C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L585C23"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Find the most significant bit in a 32-bit signed integer with a value of 0.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 585,
              "column": 23,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value to find the most significant 0-bit for.\n"
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
                    "text": "The bit position of the most significant 0-bit. -1 is returned if there\n"
                  },
                  {
                    "kind": "span",
                    "text": "are no 0-bits.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Find_Msb",
          "qualifier": "",
          "line": 616,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the most significant 1-bit in the 32-bit unsigned\n"
                },
                {
                  "kind": "span",
                  "text": "integer.\n"
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
                  "number": 616,
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
                      "text": "Find_Msb",
                      "href": "docs/vulkan__math__integers___spec.html#L616C14"
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
                      "href": "docs/vulkan__math__integers___spec.html#L616C23"
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
                      "text": "Vkm_Int",
                      "href": "docs/vulkan__math___spec.html#L58C10"
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
                  "text": "Find the most significant bit in a 32-bit unsigned integer with a value of 1.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "value",
              "line": 616,
              "column": 23,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The value to find the most significant 1-bit for.\n"
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
                    "text": "The bit position of the most significant 1-bit. -1 is returned if there\n"
                  },
                  {
                    "kind": "span",
                    "text": "are no 1-bits.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Signed_Mul_Extended",
          "qualifier": "",
          "line": 174,
          "column": 15,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation performs extended multiplication of two 32-bit signed\n"
                },
                {
                  "kind": "span",
                  "text": "integers.\n"
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
                  "number": 174,
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
                      "text": "Signed_Mul_Extended",
                      "href": "docs/vulkan__math__integers___spec.html#L174C15"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "x",
                      "href": "docs/vulkan__math__integers___spec.html#L174C35"
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
                      "text": "y",
                      "href": "docs/vulkan__math__integers___spec.html#L174C38"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
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
                      "text": ";"
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
                      "text": "                                  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "msb",
                      "href": "docs/vulkan__math__integers___spec.html#L175C35"
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
                      "text": "lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L175C40"
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
                  "text": "Multiplies two 32-bit signed integers returning two 32-bit signed \n"
                },
                {
                  "kind": "span",
                  "text": "integers representing the most significant and least significant 32 bits \n"
                },
                {
                  "kind": "span",
                  "text": "of the result of multiplication.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "x",
              "line": 174,
              "column": 35,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The left subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "y",
              "line": 174,
              "column": 38,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The right subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "msb",
              "line": 175,
              "column": 35,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The 32 most significant bits of the resulting 64-bit signed integer.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "lsb",
              "line": 175,
              "column": 40,
              "type": {
                "label": "Vulkan.Math.Vkm_Int",
                "docHref": "docs/vulkan__math___spec.html#L58C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The 32 least significant bits of the resulting 64-bit signed integer.\n"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "label": "Unsigned_Add_Carry",
          "qualifier": "",
          "line": 65,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Adds two unsigned integers modulo 32, returning the result and a carry.\n"
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
                      "text": "Unsigned_Add_Carry",
                      "href": "docs/vulkan__math__integers___spec.html#L65C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "x",
                      "href": "docs/vulkan__math__integers___spec.html#L65C33"
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
                      "text": "y",
                      "href": "docs/vulkan__math__integers___spec.html#L65C36"
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
                  "number": 66,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "carry",
                      "href": "docs/vulkan__math__integers___spec.html#L66C33"
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
                      "text": "Vkm_Uint",
                      "href": "docs/vulkan__math___spec.html#L55C10"
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
                  "text": "Adds two unsigned integers modulo 32. If the result is greater than or\n"
                },
                {
                  "kind": "span",
                  "text": "equal 2^32 the carry is 1. Otherwise the carry is 0.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "x",
              "line": 65,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "One of the addition operands.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "y",
              "line": 65,
              "column": 36,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "One of the addition operands.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "carry",
              "line": 66,
              "column": 33,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The carry value from the addition operation.\n"
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
                    "text": "The result of x + y modulo 32.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unsigned_Mul_Extended",
          "qualifier": "",
          "line": 137,
          "column": 15,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation performs extended multiplication of two 32-bit unsigned\n"
                },
                {
                  "kind": "span",
                  "text": "integers.\n"
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
                  "number": 137,
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
                      "text": "Unsigned_Mul_Extended",
                      "href": "docs/vulkan__math__integers___spec.html#L137C15"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "x",
                      "href": "docs/vulkan__math__integers___spec.html#L137C37"
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
                      "text": "y",
                      "href": "docs/vulkan__math__integers___spec.html#L137C40"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
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
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 138,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "msb",
                      "href": "docs/vulkan__math__integers___spec.html#L138C37"
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
                      "text": "lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L138C42"
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
                  "text": "Multiplies two 32-bit unsigned integers returning two 32-bit unsigned \n"
                },
                {
                  "kind": "span",
                  "text": "integers representing the most significant and least significant 32 bits \n"
                },
                {
                  "kind": "span",
                  "text": "of the result of multiplication.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "x",
              "line": 137,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The left subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "y",
              "line": 137,
              "column": 40,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The right subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "msb",
              "line": 138,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The 32 most significant bits of the resulting 64-bit unsigned integer.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "lsb",
              "line": 138,
              "column": 42,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The 32 least significant bits of the resulting 64-bit unsigned integer.\n"
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "label": "Unsigned_Sub_Borrow",
          "qualifier": "",
          "line": 100,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Subtracts two unsigned integers modulo 32, returning the result and a borrow.\n"
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
                  "number": 100,
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
                      "text": "Unsigned_Sub_Borrow",
                      "href": "docs/vulkan__math__integers___spec.html#L100C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "x",
                      "href": "docs/vulkan__math__integers___spec.html#L100C34"
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
                      "text": "y",
                      "href": "docs/vulkan__math__integers___spec.html#L100C37"
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
                  "number": 101,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "borrow",
                      "href": "docs/vulkan__math__integers___spec.html#L101C34"
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
                      "text": "Vkm_Uint",
                      "href": "docs/vulkan__math___spec.html#L55C10"
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
                  "text": "Subtracts two unsigned integers modulo 32. If x is less than y, the result\n"
                },
                {
                  "kind": "span",
                  "text": "is 2^32 plus the difference and a borrow of 1; Otherwise, the result is\n"
                },
                {
                  "kind": "span",
                  "text": "the difference and a borrow of 0.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "x",
              "line": 100,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The left subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "y",
              "line": 100,
              "column": 37,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The right subtraction operand.\n"
                    }
                  ]
                }
              ]
            },
            {
              "label": "borrow",
              "line": 101,
              "column": 34,
              "type": {
                "label": "Vulkan.Math.Vkm_Uint",
                "docHref": "docs/vulkan__math___spec.html#L55C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The borrow value from the subtraction operation.\n"
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
                    "text": "The result of x - y modulo 32.\n"
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
          "label": "Bit_Count",
          "qualifier": "(generic instantiation)",
          "line": 477,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation counts the number of 1-bits for each component of a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector.\n"
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
                  "number": 477,
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
                      "text": "Bit_Count",
                      "href": "docs/vulkan__math__integers___spec.html#L477C14"
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
                      "text": "GIT.Apply_Func_IV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bit_Count"
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
                  "text": "Applies Bit_Count() component-wise on input Vkm_GenIType, \n"
                },
                {
                  "kind": "span",
                  "text": "returning a Vkm_GenIType vector of counts for each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L9992C14"
          }
        },
        {
          "label": "Bit_Count",
          "qualifier": "(generic instantiation)",
          "line": 505,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation counts the number of 1-bits for each component of a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenUType vector.\n"
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
                  "number": 505,
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
                      "text": "Bit_Count",
                      "href": "docs/vulkan__math__integers___spec.html#L505C14"
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
                      "text": "Apply_Func_IVU_RVI"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bit_Count"
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
                  "text": "Applies Bit_Count() component-wise on input Vkm_GenUType, \n"
                },
                {
                  "kind": "span",
                  "text": "returning a Vkm_GenIType vector of counts for each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenIType.Apply_Func_IVU_RVI",
            "docHref": "docs/vulkan__math__genitype___spec.html#L133C14"
          }
        },
        {
          "label": "Bitfield_Extract",
          "qualifier": "(generic instantiation)",
          "line": 236,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation extracts a bitfield vector from a Vkm_GenIType vector.\n"
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
                  "number": 236,
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
                      "text": "Bitfield_Extract",
                      "href": "docs/vulkan__math__integers___spec.html#L236C14"
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
                      "text": "GIT.Apply_Func_IV_IS_IS_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Extract"
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
                  "text": "Applies Bitfield_Extract() component-wise on an input Vkm_GenIType\n"
                },
                {
                  "kind": "span",
                  "text": "vector and two Vkm_Int scalars, returning a Vkm_GenIType vector of the\n"
                },
                {
                  "kind": "span",
                  "text": "extracted bitfields.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IS_IS_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10099C14"
          }
        },
        {
          "label": "Bitfield_Extract",
          "qualifier": "(generic instantiation)",
          "line": 286,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation extracts a bitfield vector from a Vkm_GenUType vector.\n"
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
                      "text": "Bitfield_Extract",
                      "href": "docs/vulkan__math__integers___spec.html#L286C14"
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
                      "text": "Apply_Func_IVU_ISI_ISI_RVU"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Extract"
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
                  "text": "Applies Bitfield_Extract() component-wise on an input Vkm_GenUType\n"
                },
                {
                  "kind": "span",
                  "text": "vector and two Vkm_Int scalars, returning a Vkm_GenUType vector of the\n"
                },
                {
                  "kind": "span",
                  "text": "extracted bitfields.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenUType.Apply_Func_IVU_ISI_ISI_RVU",
            "docHref": "docs/vulkan__math__genutype___spec.html#L134C14"
          }
        },
        {
          "label": "Bitfield_Insert",
          "qualifier": "(generic instantiation)",
          "line": 333,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation inserts a bitfield vector into a Vkm_GenIType vector.\n"
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
                  "number": 333,
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
                      "text": "Bitfield_Insert",
                      "href": "docs/vulkan__math__integers___spec.html#L333C14"
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
                      "text": "GIT.Apply_Func_IV_IV_IS_IS_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Insert"
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
                  "text": "Applies Bitfield_Insert() component-wise on two input Vkm_GenIType\n"
                },
                {
                  "kind": "span",
                  "text": "vectors and two Vkm_Int scalars, returning a Vkm_GenIType vector of the\n"
                },
                {
                  "kind": "span",
                  "text": "base vector with inserted bitfields.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IV_IS_IS_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10212C14"
          }
        },
        {
          "label": "Bitfield_Insert",
          "qualifier": "(generic instantiation)",
          "line": 381,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation inserts a bitfield vector into a Vkm_GenUType vector.\n"
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
                  "number": 381,
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
                      "text": "Bitfield_Insert",
                      "href": "docs/vulkan__math__integers___spec.html#L381C14"
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
                      "text": "Apply_Func_IVU_IVU_ISI_ISI_RVU"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Insert"
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
                  "text": "Applies Bitfield_Insert() component-wise on two input Vkm_GenUType\n"
                },
                {
                  "kind": "span",
                  "text": "vectors and two Vkm_Int scalars, returning a Vkm_GenUType vector of the\n"
                },
                {
                  "kind": "span",
                  "text": "base vector with inserted bitfields.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenUType.Apply_Func_IVU_IVU_ISI_ISI_RVU",
            "docHref": "docs/vulkan__math__genutype___spec.html#L167C14"
          }
        },
        {
          "label": "Bitfield_Reverse",
          "qualifier": "(generic instantiation)",
          "line": 415,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation reverses the binary representation of the components of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector.\n"
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
                  "number": 415,
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
                      "text": "Bitfield_Reverse",
                      "href": "docs/vulkan__math__integers___spec.html#L415C14"
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
                      "text": "GIT.Apply_Func_IV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Reverse"
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
                  "text": "Applies Bitfield_Reverse() component-wise on input Vkm_GenIType, \n"
                },
                {
                  "kind": "span",
                  "text": "returning a Vkm_GenIType vector of the reversed components.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L9992C14"
          }
        },
        {
          "label": "Bitfield_Reverse",
          "qualifier": "(generic instantiation)",
          "line": 449,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation reverses the binary representation of the components of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenUType vector.\n"
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
                  "number": 449,
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
                      "text": "Bitfield_Reverse",
                      "href": "docs/vulkan__math__integers___spec.html#L449C14"
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
                      "text": "GUT.Apply_Func_IV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bitfield_Reverse"
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
                  "text": "Applies Bitfield_Reverse() component-wise on input Vkm_GenUType, \n"
                },
                {
                  "kind": "span",
                  "text": "returning a Vkm_GenUType vector of the reversed components.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L9992C14"
          }
        },
        {
          "label": "Find_Lsb",
          "qualifier": "(generic instantiation)",
          "line": 536,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the least significant 1-bit for each component of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector.\n"
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
                  "number": 536,
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
                      "text": "Find_Lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L536C14"
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
                      "text": "GIT.Apply_Func_IV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Find_Lsb"
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
                  "text": "Applies Find_Lsb() component-wise on input Vkm_GenIType, returning a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector of the bit positions for the least significant 1-bit \n"
                },
                {
                  "kind": "span",
                  "text": "in each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L9992C14"
          }
        },
        {
          "label": "Find_Lsb",
          "qualifier": "(generic instantiation)",
          "line": 567,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the least significant 1-bit for each component of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenUType vector.\n"
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
                  "number": 567,
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
                      "text": "Find_Lsb",
                      "href": "docs/vulkan__math__integers___spec.html#L567C14"
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
                      "text": "Apply_Func_IVU_RVI"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Find_Lsb"
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
                  "text": "Applies Find_Lsb() component-wise on input Vkm_GenUType, returning a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector of the bit positions for the least significant 1-bit \n"
                },
                {
                  "kind": "span",
                  "text": "in each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenIType.Apply_Func_IVU_RVI",
            "docHref": "docs/vulkan__math__genitype___spec.html#L133C14"
          }
        },
        {
          "label": "Find_Msb",
          "qualifier": "(generic instantiation)",
          "line": 598,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the most significant 0-bit for each component of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector.\n"
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
                  "number": 598,
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
                      "text": "Find_Msb",
                      "href": "docs/vulkan__math__integers___spec.html#L598C14"
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
                      "text": "GIT.Apply_Func_IV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Find_Msb"
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
                  "text": "Applies Find_Msb() component-wise on input Vkm_GenIType, returning a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector of the bit positions for the most significant 0-bit \n"
                },
                {
                  "kind": "span",
                  "text": "in each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L9992C14"
          }
        },
        {
          "label": "Find_Msb",
          "qualifier": "(generic instantiation)",
          "line": 629,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation finds the most significant 1-bit for each component of a\n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenUType vector.\n"
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
                  "number": 629,
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
                      "text": "Find_Msb",
                      "href": "docs/vulkan__math__integers___spec.html#L629C14"
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
                      "text": "Apply_Func_IVU_RVI"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Find_Msb"
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
                  "text": "Applies Find_Msb() component-wise on input Vkm_GenUType, returning a \n"
                },
                {
                  "kind": "span",
                  "text": "Vkm_GenIType vector of the bit positions for the most significant 0-bit \n"
                },
                {
                  "kind": "span",
                  "text": "in each component.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenIType.Apply_Func_IVU_RVI",
            "docHref": "docs/vulkan__math__genitype___spec.html#L133C14"
          }
        },
        {
          "label": "Signed_Mul_Extended",
          "qualifier": "(generic instantiation)",
          "line": 187,
          "column": 15,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation performs extended multiplication of two 32-bit signed\n"
                },
                {
                  "kind": "span",
                  "text": "integers.\n"
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
                  "number": 187,
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
                      "text": "Signed_Mul_Extended",
                      "href": "docs/vulkan__math__integers___spec.html#L187C15"
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
                      "text": "GIT.Apply_Func_IV_IV_OV_OV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Signed_Mul_Extended"
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
                  "text": "Applies Unsigned_Mul_Extended() component-wise on two input Vkm_GenIType\n"
                },
                {
                  "kind": "span",
                  "text": "vectors and two output Vkm_GenIType vectors.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IV_OV_OV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10184C15"
          }
        },
        {
          "label": "Unsigned_Add_Carry",
          "qualifier": "(generic instantiation)",
          "line": 77,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Adds two unsigned integers modulo 32, returning the result and a carry.\n"
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
                      "text": "Unsigned_Add_Carry",
                      "href": "docs/vulkan__math__integers___spec.html#L77C14"
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
                      "text": "GUT.Apply_Func_IV_IV_OV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unsigned_Add_Carry"
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
                  "text": "Applies Unsigned_Add_Carry() component-wise on three Vkm_GenUType vectors,\n"
                },
                {
                  "kind": "span",
                  "text": "returning the result of addition as a Vkm_GenUType vector.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IV_OV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10155C14"
          }
        },
        {
          "label": "Unsigned_Mul_Extended",
          "qualifier": "(generic instantiation)",
          "line": 149,
          "column": 15,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation performs extended multiplication of two Vkm_GenUType vectors.\n"
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
                  "number": 149,
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
                      "text": "Unsigned_Mul_Extended",
                      "href": "docs/vulkan__math__integers___spec.html#L149C15"
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
                      "text": "GUT.Apply_Func_IV_IV_OV_OV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unsigned_Mul_Extended"
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
                  "text": "Applies Unsigned_Mul_Extended() component-wise on two input Vkm_GenUType\n"
                },
                {
                  "kind": "span",
                  "text": "vectors and two output Vkm_GenUType vectors.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IV_OV_OV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10184C15"
          }
        },
        {
          "label": "Unsigned_Sub_Borrow",
          "qualifier": "(generic instantiation)",
          "line": 112,
          "column": 14,
          "src": "srcs/vulkan-math-integers.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Subtracts two unsigned integers modulo 32, returning the result and a borrow.\n"
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
                      "text": "Unsigned_Sub_Borrow",
                      "href": "docs/vulkan__math__integers___spec.html#L112C14"
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
                      "text": "GUT.Apply_Func_IV_IV_OV_RV"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unsigned_Sub_Borrow"
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
                  "text": "Applies Unsigned_Sub_Borrow() component-wise on three Vkm_GenUType vectors,\n"
                },
                {
                  "kind": "span",
                  "text": "returning the result of addition as a Vkm_GenUType vector.\n"
                }
              ]
            }
          ],
          "instantiation": {
            "label": "Vulkan.Math.GenType.Apply_Func_IV_IV_OV_RV",
            "docHref": "docs/vulkan__math__gentype___spec.html#L10155C14"
          }
        }
      ],
      "label": "Generic instantiations"
    }
  ]
};