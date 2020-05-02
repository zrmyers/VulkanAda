GNATdoc.Documentation = {
  "label": "Vulkan.Math.Packing",
  "qualifier": "",
  "summary": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "This package provides GLSL Floating Point Packing and Unpacking functions.\n"
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
          "text": "All floating point pack and unpack functions op\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Pack_Double_2x32",
          "qualifier": "",
          "line": 363,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs a Vkm_Uvec2 into a 64-bit Vkm_Double value.\n"
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
                  "number": 363,
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
                      "text": "Pack_Double_2x32",
                      "href": "docs/vulkan__math__packing___spec.html#L363C14"
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
                  "number": 364,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L364C9"
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
                      "text": "Vkm_Uvec2",
                      "href": "docs/vulkan__math__uvec2___spec.html#L38C13"
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
                      "text": "Vkm_Double",
                      "href": "docs/vulkan__math___spec.html#L64C10"
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
                  "text": "Each component of the Vkm_Uvec2 input is packed into a 32-bit bitfield \n"
                },
                {
                  "kind": "span",
                  "text": "of a Vkm_Double.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The Vkm_Uvec2 is packed into the Vkm_Double as follows:\n"
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
                      "text": "bits      | 63 62 ... 33 32 | 31 30 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 364,
              "column": 9,
              "type": {
                "label": "Vulkan.Math.Uvec2.Vkm_Uvec2",
                "docHref": "docs/vulkan__math__uvec2___spec.html#L38C13"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The Vkm_Uvec2 vector that is packed into the double.\n"
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
                    "text": "The Vkm_Double that contains the packed Vkm_Uvec2 value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Pack_Half_2x16",
          "qualifier": "",
          "line": 307,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs components of a Vkm_Vec2 as half-floats into\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 307,
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
                      "text": "Pack_Half_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L307C14"
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
                  "number": 308,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L308C9"
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
                  "text": "Each component of the Vkm_Vec2 is converted to a half-precision floating\n"
                },
                {
                  "kind": "span",
                  "text": "point number and then packed into a 16-bit field of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The floating point representations are shown below for reference:\n"
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
                      "text": "bits         | Sign | Exponent | Significand |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Half-Float   | 15   | 14 .. 10 |   9 .. 0    |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Single-Float | 31   | 30 .. 23 |  22 .. 0    |"
                    }
                  ]
                },
                {
                  "number": 4,
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
                  "text": "Conversion is performed by copying the least significant bits of the fields\n"
                },
                {
                  "kind": "span",
                  "text": "of the single-precision floating point number to the corresponding fields\n"
                },
                {
                  "kind": "span",
                  "text": "of the half-precision floating point number.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The vector is packed as follows into the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 308,
              "column": 9,
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
                      "text": "The Vkm_Vec2 vector that is packed into the Vkm_Uint.\n"
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
                    "text": "The Vkm_Uint that contains the packed vector.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Pack_Signed_Normalized_2x16",
          "qualifier": "",
          "line": 100,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs a normalized Vkm_Vec2 with signed components into\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                      "text": "Pack_Signed_Normalized_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L100C14"
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
                  "number": 101,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L101C9"
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
                  "text": "Each component of the signed normalized input Vkm_Vec2 is packed into\n"
                },
                {
                  "kind": "span",
                  "text": "an 16-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The following conversion function is used to transform each floating\n"
                },
                {
                  "kind": "span",
                  "text": "point component into an 16-bit bitfield, where c is a component of the vector:\n"
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
                      "text": "int16_c := round( clamp ( c, -1, 1) * 32767.0)"
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
                  "text": "The packed vector is formatted as follows in the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 101,
              "column": 9,
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
                      "text": "The normalized Vkm_Vec2 value to pack into an unsigned integer.\n"
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
                    "text": "The unsigned integer value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Pack_Signed_Normalized_4x8",
          "qualifier": "",
          "line": 156,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs a normalized Vkm_Vec4 with signed components into\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 156,
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
                      "text": "Pack_Signed_Normalized_4x8",
                      "href": "docs/vulkan__math__packing___spec.html#L156C14"
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
                  "number": 157,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L157C9"
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
                  "text": "Each component of the signed normalized input Vkm_Vec4 is packed into\n"
                },
                {
                  "kind": "span",
                  "text": "an 8-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The following conversion function is used to transform each floating\n"
                },
                {
                  "kind": "span",
                  "text": "point component into an 8-bit bitfield, where c is a component of the vector:\n"
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
                      "text": "int8_c := round( clamp ( c, -1, 1) * 127.0)"
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
                  "text": "The packed vector is formatted as follows in the unsigned integer:\n"
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
                      "text": "bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |     w     |     z     |     y    |    x    |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 157,
              "column": 9,
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
                      "text": "The normalized Vkm_Vec4 value to pack into an unsigned integer.\n"
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
                    "text": "The unsigned integer value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Pack_Unsigned_Normalized_2x16",
          "qualifier": "",
          "line": 72,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs a normalized Vkm_Vec2 with unsigned components into\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 72,
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
                      "text": "Pack_Unsigned_Normalized_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L72C14"
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
                  "number": 73,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L73C9"
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
                  "text": "Each component of the unsigned normalized input Vkm_Vec2 is packed into\n"
                },
                {
                  "kind": "span",
                  "text": "an 16-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The following conversion function is used to transform each floating\n"
                },
                {
                  "kind": "span",
                  "text": "point component into an 16-bit bitfield, where c is a component of the vector:\n"
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
                      "text": "uint16_c := round( clamp ( c, 0, 1) * 65535.0)"
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
                  "text": "The packed vector is formatted as follows in the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 73,
              "column": 9,
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
                      "text": "The normalized Vkm_Vec2 value to pack into an unsigned integer.\n"
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
                    "text": "The unsigned integer value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Pack_Unsigned_Normalized_4x8",
          "qualifier": "",
          "line": 128,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation packs a normalized Vkm_Vec4 with unsigned components into\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 128,
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
                      "text": "Pack_Unsigned_Normalized_4x8",
                      "href": "docs/vulkan__math__packing___spec.html#L128C14"
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
                  "number": 129,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "vector",
                      "href": "docs/vulkan__math__packing___spec.html#L129C9"
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
                  "text": "Each component of the unsigned normalized input Vkm_Vec4 is packed into\n"
                },
                {
                  "kind": "span",
                  "text": "an 8-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The following conversion function is used to transform each floating\n"
                },
                {
                  "kind": "span",
                  "text": "point component into an 8-bit bitfield, where c is a component of the vector:\n"
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
                      "text": "uint8_c := round( clamp ( c, 0, 1) * 255.0)"
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
                  "text": "The packed vector is formatted as follows in the unsigned integer:\n"
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
                      "text": "bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |     w     |     z     |     y    |    x    |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "vector",
              "line": 129,
              "column": 9,
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
                      "text": "The normalized Vkm_Vec4 value to pack into an unsigned integer.\n"
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
                    "text": "The unsigned integer value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Double_2x32",
          "qualifier": "",
          "line": 385,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation unpacks a Vkm_Uvec2 from a 64-bit Vkm_Double value.\n"
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
                  "number": 385,
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
                      "text": "Unpack_Double_2x32",
                      "href": "docs/vulkan__math__packing___spec.html#L385C14"
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
                  "number": 386,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L386C9"
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
                      "text": "Vkm_Double",
                      "href": "docs/vulkan__math___spec.html#L64C10"
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
                      "text": "Vkm_Uvec2",
                      "href": "docs/vulkan__math__uvec2___spec.html#L38C13"
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
                  "text": "Each component of the Vkm_Uvec2 output is unpacked from a 32-bit bitfield \n"
                },
                {
                  "kind": "span",
                  "text": "of a Vkm_Double.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The Vkm_Uvec2 is unpacked from the Vkm_Double as follows:\n"
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
                      "text": "bits      | 63 62 ... 33 32 | 31 30 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 386,
              "column": 9,
              "type": {
                "label": "Vulkan.Math.Vkm_Double",
                "docHref": "docs/vulkan__math___spec.html#L64C10"
              },
              "description": [
                {
                  "kind": "paragraph",
                  "children": [
                    {
                      "kind": "span",
                      "text": "The Vkm_Double that contains the packed Vkm_Uvec2.\n"
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
                    "text": "The Vkm_Uvec2 unpacked from the Vkm_Double.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Half_2x16",
          "qualifier": "",
          "line": 339,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation upacks a Vkm_Vec2 vector from an unsigned integer.\n"
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
                  "number": 339,
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
                      "text": "Unpack_Half_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L339C14"
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
                  "number": 340,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L340C9"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
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
                  "text": "Each component of the Vkm_Vec2 is converted from a half-precision floating\n"
                },
                {
                  "kind": "span",
                  "text": "point number after being unpacked from a 16-bit field of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The packed vector is extracted as follows from the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
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
                  "text": "The floating point representations are shown below for reference:\n"
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
                      "text": "Fields       | Sign | Exponent | Significand |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Half-Float   | 15   | 14 .. 10 |   9 .. 0    |"
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "Single-Float | 31   | 30 .. 23 |  22 .. 0    |"
                    }
                  ]
                },
                {
                  "number": 4,
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
                  "text": "Conversion is performed by copying the fields of the half-precision \n"
                },
                {
                  "kind": "span",
                  "text": "floating point number to the least significant bits of the corresponding \n"
                },
                {
                  "kind": "span",
                  "text": "fields of the single-precision floating point number.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 340,
              "column": 9,
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
                      "text": "The Vkm_Uint that contains the packed vector.\n"
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
                    "text": "The unpacked Vkm_Vec2.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Signed_Normalized_2x16",
          "qualifier": "",
          "line": 214,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation unpacks a normalized Vkm_Vec2 with signed components from\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                      "text": "Unpack_Signed_Normalized_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L214C14"
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
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L215C9"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
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
                  "text": "Each component of the signed normalized output Vkm_Vec2 is unpacked from\n"
                },
                {
                  "kind": "span",
                  "text": "a 16-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The unpacked vector is extracted as follows from the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
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
                  "text": "The following conversion function is used to transform each 16-bit \n"
                },
                {
                  "kind": "span",
                  "text": "bitfield into a floating point value, where c is a component of the vector,\n"
                },
                {
                  "kind": "span",
                  "text": "and uint16_c is the 16-bit packed component:\n"
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
                      "text": "c := clamp(uint16_c / 32767.0, -1, 1)"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 215,
              "column": 9,
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
                      "text": "The unsigned integer that contains the packed Vkm_Vec2.\n"
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
                    "text": "The unpacked signed normalized Vkm_Vec2 value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Signed_Normalized_4x8",
          "qualifier": "",
          "line": 272,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation unpacks a normalized Vkm_Vec4 with signed components from\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 272,
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
                      "text": "Unpack_Signed_Normalized_4x8",
                      "href": "docs/vulkan__math__packing___spec.html#L272C14"
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
                  "number": 273,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L273C9"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "text": "Each component of the signed normalized output Vkm_Vec4 is unpacked from\n"
                },
                {
                  "kind": "span",
                  "text": "an 8-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The unpacked vector is extracted as follows from the unsigned integer:\n"
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
                      "text": "bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |     w     |     z     |     y    |    x    |"
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
                  "text": "The following conversion function is used to transform each 8-bit \n"
                },
                {
                  "kind": "span",
                  "text": "bitfield into a floating point value, where c is a component of the vector,\n"
                },
                {
                  "kind": "span",
                  "text": "and uint8_c is the 8-bit packed component:\n"
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
                      "text": "c := clamp(uint8_c / 127.0, -1, 1)"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 273,
              "column": 9,
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
                      "text": "The unsigned integer that contains the packed Vkm_Vec4.\n"
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
                    "text": "The unpacked signed normalized Vkm_Vec4 value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Unsigned_Normalized_2x16",
          "qualifier": "",
          "line": 185,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation unpacks a normalized Vkm_Vec2 with unsigned components from\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                      "text": "Unpack_Unsigned_Normalized_2x16",
                      "href": "docs/vulkan__math__packing___spec.html#L185C14"
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
                  "number": 186,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L186C9"
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
                      "text": "Vkm_Vec2",
                      "href": "docs/vulkan__math__vec2___spec.html#L39C13"
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
                  "text": "Each component of the unsigned normalized output Vkm_Vec2 is unpacked from\n"
                },
                {
                  "kind": "span",
                  "text": "a 16-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The unpacked vector is extracted as follows from the unsigned integer:\n"
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
                      "text": "bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |      y          |        x      |"
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
                  "text": "The following conversion function is used to transform each 16-bit \n"
                },
                {
                  "kind": "span",
                  "text": "bitfield into a floating point value, where c is a component of the vector,\n"
                },
                {
                  "kind": "span",
                  "text": "and uint16_c is the 16-bit packed component:\n"
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
                      "text": "c := uint16_c / 65535.0"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 186,
              "column": 9,
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
                      "text": "The unsigned integer that contains the packed Vkm_Vec2.\n"
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
                    "text": "The unpacked signed normalized Vkm_Vec2 value.\n"
                  }
                ]
              }
            ]
          }
        },
        {
          "label": "Unpack_Unsigned_Normalized_4x8",
          "qualifier": "",
          "line": 243,
          "column": 14,
          "src": "srcs/vulkan-math-packing.ads.html",
          "summary": [
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "This operation unpacks a normalized Vkm_Vec4 with unsigned components from\n"
                },
                {
                  "kind": "span",
                  "text": "a 32-bit unsigned integer.\n"
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
                  "number": 243,
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
                      "text": "Unpack_Unsigned_Normalized_4x8",
                      "href": "docs/vulkan__math__packing___spec.html#L243C14"
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
                  "number": 244,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "packed",
                      "href": "docs/vulkan__math__packing___spec.html#L244C9"
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
                      "text": "Vkm_Vec4",
                      "href": "docs/vulkan__math__vec4___spec.html#L44C13"
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
                  "text": "Each component of the unsigned normalized output Vkm_Vec4 is unpacked from\n"
                },
                {
                  "kind": "span",
                  "text": "an 8-bit bitfield of an unsigned integer.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The unpacked vector is extracted as follows from the unsigned integer:\n"
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
                      "text": "bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |"
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "component |     w     |     z     |     y    |    x    |"
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
                  "text": "The following conversion function is used to transform each 8-bit \n"
                },
                {
                  "kind": "span",
                  "text": "bitfield into a floating point value, where c is a component of the vector,\n"
                },
                {
                  "kind": "span",
                  "text": "and uint8_c is the 8-bit packed component:\n"
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
                      "text": "c := uint8_c / 256.0"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "packed",
              "line": 244,
              "column": 9,
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
                      "text": "The unsigned integer that contains the packed Vkm_Vec4.\n"
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
                    "text": "The unpacked unsigned normalized Vkm_Vec4 value.\n"
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