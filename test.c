debug: int
assembly: int
token: int





text: (int*), stack: (int*)
old_text: (int*)
data: (char*)
idmain: (int*)
src: (char*), old_src: (char*)
poolsize: int
pc: (int*), bp: (int*), sp: (int*), ax: int, cycle: int
current_id: (int*), symbols: (int*), line: int, token_val: int
basetype: int
expr_type: int
index_of_bp: int
Function next :
void (){
    	last_pos: (char*)
    	hash: int
    	while ((token = *src)) {{
  	++src;
  	if ((token == <10>)) {{
        	if (assembly) {{
              	printf("%d: %.*s", line, (src - old_src), old_src);
              	(old_src = src);
              	while ((old_text <= text)) {{
  	printf("%8.4s", &"LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[(*++old_text * 5)]);
  	if ((*old_text <= ADJ)) {printf(" %d
", *++old_text);{printf("
");}
}}
          }
        	++line;
    }{if ((token == '#')) {{
          	while (((*src != 0) && (*src != <10>))) {{
  	src++;
}}
      }{if (((((token >= 'a') && (token <= 'z')) || ((token >= 'A') && (token <= 'Z'))) || (token == '_'))) {{
            	(last_pos = (src - 1));
            	(hash = token);
            	while ((((((*src >= 'a') && (*src <= 'z')) || ((*src >= 'A') && (*src <= 'Z'))) || ((*src >= '0') && (*src <= '9'))) || (*src == '_'))) {{
  	(hash = ((hash * 147) + *src));
  	src++;
}}
            	(current_id = symbols);
            	while (current_id[Token]) {{
  	if (((current_id[Hash] == hash) && !memcmp((char*)(current_id[Name]), last_pos, (src - last_pos)))) {{
        	(token = current_id[Token]);
        	return;
    }
  	(current_id = (current_id + IdSize));
}}
            	(current_id[Name] = int(last_pos));
            	(current_id[Hash] = hash);
            	(token = (current_id[Token] = Id));
            	return;
        }{if (((token >= '0') && (token <= '9'))) {{
              	(token_val = (token - '0'));
              	if ((token_val > 0)) {{
                    	while (((*src >= '0') && (*src <= '9'))) {{
  	(token_val = (((token_val * 10) + *src++) - '0'));
}}
                }{{
                    	if (((*src == 'x') || (*src == 'X'))) {{
                          	(token = *++src);
                          	while (((((token >= '0') && (token <= '9')) || ((token >= 'a') && (token <= 'f'))) || ((token >= 'A') && (token <= 'F')))) {{
  	(token_val = (((token_val * 16) + (token & 15)) + (token >= 'A')?9:0));
  	(token = *++src);
}}
                      }{{
                          	while (((*src >= '0') && (*src <= '7'))) {{
  	(token_val = (((token_val * 8) + *src++) - '0'));
}}
                      }}
                }}
              	(token = Num);
              	return;
          }{if ((token == '/')) {{
                	if ((*src == '/')) {{
                      	while (((*src != 0) && (*src != <10>))) {{
  	++src;
}}
                  }{{
                      	(token = Div);
                      	return;
                  }}
            }{if (((token == '"') || (token == '''))) {{
                  	(last_pos = data);
                  	while (((*src != 0) && (*src != token))) {{
  	(token_val = *src++);
  	if ((token_val == '\')) {{
        	(token_val = *src++);
        	if ((token_val == 'n')) {{
              	(token_val = <10>);
          }
    }
  	if ((token == '"')) {{
        	(*data++ = token_val);
    }
}}
                  	src++;
                  	if ((token == '"')) {{
                        	(token_val = int(last_pos));
                    }{{
                        	(token = Num);
                    }}
                  	return;
              }{if ((token == '=')) {{
                    	if ((*src == '=')) {{
                          	src++;
                          	(token = Eq);
                      }{{
                          	(token = Assign);
                      }}
                    	return;
                }{if ((token == '+')) {{
                      	if ((*src == '+')) {{
                            	src++;
                            	(token = Inc);
                        }{{
                            	(token = Add);
                        }}
                      	return;
                  }{if ((token == '-')) {{
                        	if ((*src == '-')) {{
                              	src++;
                              	(token = Dec);
                          }{{
                              	(token = Sub);
                          }}
                        	return;
                    }{if ((token == '!')) {{
                          	if ((*src == '=')) {{
                                	src++;
                                	(token = Ne);
                            }
                          	return;
                      }{if ((token == '<')) {{
                            	if ((*src == '=')) {{
                                  	src++;
                                  	(token = Le);
                              }{if ((*src == '<')) {{
                                    	src++;
                                    	(token = Shl);
                                }{{
                                    	(token = Lt);
                                }}}
                            	return;
                        }{if ((token == '>')) {{
                              	if ((*src == '=')) {{
                                    	src++;
                                    	(token = Ge);
                                }{if ((*src == '>')) {{
                                      	src++;
                                      	(token = Shr);
                                  }{{
                                      	(token = Gt);
                                  }}}
                              	return;
                          }{if ((token == '|')) {{
                                	if ((*src == '|')) {{
                                      	src++;
                                      	(token = Lor);
                                  }{{
                                      	(token = Or);
                                  }}
                                	return;
                            }{if ((token == '&')) {{
                                  	if ((*src == '&')) {{
                                        	src++;
                                        	(token = Lan);
                                    }{{
                                        	(token = And);
                                    }}
                                  	return;
                              }{if ((token == '^')) {{
                                    	(token = Xor);
                                    	return;
                                }{if ((token == '%')) {{
                                      	(token = Mod);
                                      	return;
                                  }{if ((token == '*')) {{
                                        	(token = Mul);
                                        	return;
                                    }{if ((token == '[')) {{
                                          	(token = Brak);
                                          	return;
                                      }{if ((token == '?')) {{
                                            	(token = Cond);
                                            	return;
                                        }{if ((((((((((token == '~') || (token == ';')) || (token == '{')) || (token == '}')) || (token == '(')) || (token == ')')) || (token == ']')) || (token == ',')) || (token == ':'))) {{
                                              	return;
                                          }}}}}}}}}}}}}}}}}}}}
}}
}
Function match :
void (int tk){
    	if ((token == tk)) {{
          	next();
      }{{
          	printf("%d: expected token: %d
", line, tk);
          	exit(-1);
      }}
}
Function expression :
void (int level){
    	id: (int*)
    	tmp: int
    	addr: (int*)
    	{
        	if (!token) {{
              	printf("%d: unexpected token EOF of expression
", line);
              	exit(-1);
          }
        	if ((token == Num)) {{
              	match(Num);
              	(*++text = IMM);
              	(*++text = token_val);
              	(expr_type = INT);
          }{if ((token == '"')) {{
                	(*++text = IMM);
                	(*++text = token_val);
                	match('"');
                	while ((token == '"')) {{
  	match('"');
}}
                	(data = (char*)(((int(data) + sizeof(int)) & -sizeof(int))));
                	(expr_type = PTR);
            }{if ((token == Sizeof)) {{
                  	match(Sizeof);
                  	match('(');
                  	(expr_type = INT);
                  	if ((token == Int)) {{
                        	match(Int);
                    }{if ((token == Char)) {{
                          	match(Char);
                          	(expr_type = CHAR);
                      }}
                  	while ((token == Mul)) {{
  	match(Mul);
  	(expr_type = (expr_type + PTR));
}}
                  	match(')');
                  	(*++text = IMM);
                  	(*++text = (expr_type == CHAR)?sizeof(char):sizeof(int));
                  	(expr_type = INT);
              }{if ((token == Id)) {{
                    	match(Id);
                    	(id = current_id);
                    	if ((token == '(')) {{
                          	match('(');
                          	(tmp = 0);
                          	while ((token != ')')) {{
  	expression(Assign);
  	(*++text = PUSH);
  	tmp++;
  	if ((token == ',')) {{
        	match(',');
    }
}}
                          	match(')');
                          	if ((id[Class] == Sys)) {{
                                	(*++text = id[Value]);
                            }{if ((id[Class] == Fun)) {{
                                  	(*++text = CALL);
                                  	(*++text = id[Value]);
                              }{{
                                  	printf("%d: bad function call
", line);
                                  	exit(-1);
                              }}}
                          	if ((tmp > 0)) {{
                                	(*++text = ADJ);
                                	(*++text = tmp);
                            }
                          	(expr_type = id[Type]);
                      }{if ((id[Class] == Num)) {{
                            	(*++text = IMM);
                            	(*++text = id[Value]);
                            	(expr_type = INT);
                        }{{
                            	if ((id[Class] == Loc)) {{
                                  	(*++text = LEA);
                                  	(*++text = (index_of_bp - id[Value]));
                              }{if ((id[Class] == Glo)) {{
                                    	(*++text = IMM);
                                    	(*++text = id[Value]);
                                }{{
                                    	printf("%d: undefined variable
", line);
                                    	exit(-1);
                                }}}
                            	(expr_type = id[Type]);
                            	(*++text = (expr_type == CHAR)?LC:LI);
                        }}}
                }{if ((token == '(')) {{
                      	match('(');
                      	if (((token == Int) || (token == Char))) {{
                            	(tmp = (token == Char)?CHAR:INT);
                            	match(token);
                            	while ((token == Mul)) {{
  	match(Mul);
  	(tmp = (tmp + PTR));
}}
                            	match(')');
                            	expression(Inc);
                            	(expr_type = tmp);
                        }{{
                            	expression(Assign);
                            	match(')');
                        }}
                  }{if ((token == Mul)) {{
                        	match(Mul);
                        	expression(Inc);
                        	if ((expr_type >= PTR)) {{
                              	(expr_type = (expr_type - PTR));
                          }{{
                              	printf("%d: bad dereference
", line);
                              	exit(-1);
                          }}
                        	(*++text = (expr_type == CHAR)?LC:LI);
                    }{if ((token == And)) {{
                          	match(And);
                          	expression(Inc);
                          	if (((*text == LC) || (*text == LI))) {{
                                	text--;
                            }{{
                                	printf("%d: bad address of
", line);
                                	exit(-1);
                            }}
                          	(expr_type = (expr_type + PTR));
                      }{if ((token == '!')) {{
                            	match('!');
                            	expression(Inc);
                            	(*++text = PUSH);
                            	(*++text = IMM);
                            	(*++text = 0);
                            	(*++text = EQ);
                            	(expr_type = INT);
                        }{if ((token == '~')) {{
                              	match('~');
                              	expression(Inc);
                              	(*++text = PUSH);
                              	(*++text = IMM);
                              	(*++text = -1);
                              	(*++text = XOR);
                              	(expr_type = INT);
                          }{if ((token == Add)) {{
                                	match(Add);
                                	expression(Inc);
                                	(expr_type = INT);
                            }{if ((token == Sub)) {{
                                  	match(Sub);
                                  	if ((token == Num)) {{
                                        	(*++text = IMM);
                                        	(*++text = -token_val);
                                        	match(Num);
                                    }{{
                                        	(*++text = IMM);
                                        	(*++text = -1);
                                        	(*++text = PUSH);
                                        	expression(Inc);
                                        	(*++text = MUL);
                                    }}
                                  	(expr_type = INT);
                              }{if (((token == Inc) || (token == Dec))) {{
                                    	(tmp = token);
                                    	match(token);
                                    	expression(Inc);
                                    	if ((*text == LC)) {{
                                          	(*text = PUSH);
                                          	(*++text = LC);
                                      }{if ((*text == LI)) {{
                                            	(*text = PUSH);
                                            	(*++text = LI);
                                        }{{
                                            	printf("%d: bad lvalue of pre-increment
", line);
                                            	exit(-1);
                                        }}}
                                    	(*++text = PUSH);
                                    	(*++text = IMM);
                                    	(*++text = (expr_type > PTR)?sizeof(int):sizeof(char));
                                    	(*++text = (tmp == Inc)?ADD:SUB);
                                    	(*++text = (expr_type == CHAR)?SC:SI);
                                }{{
                                    	printf("%d: bad expression
", line);
                                    	exit(-1);
                                }}}}}}}}}}}}}
    }
    	{
        	while ((token >= level)) {{
  	(tmp = expr_type);
  	if ((token == Assign)) {{
        	match(Assign);
        	if (((*text == LC) || (*text == LI))) {{
              	(*text = PUSH);
          }{{
              	printf("%d: bad lvalue in assignment
", line);
              	exit(-1);
          }}
        	expression(Assign);
        	(expr_type = tmp);
        	(*++text = (expr_type == CHAR)?SC:SI);
    }{if ((token == Cond)) {{
          	match(Cond);
          	(*++text = JZ);
          	(addr = ++text);
          	expression(Assign);
          	if ((token == ':')) {{
                	match(':');
            }{{
                	printf("%d: missing colon in conditional
", line);
                	exit(-1);
            }}
          	(*addr = int((text + 3)));
          	(*++text = JMP);
          	(addr = ++text);
          	expression(Cond);
          	(*addr = int((text + 1)));
      }{if ((token == Lor)) {{
            	match(Lor);
            	(*++text = JNZ);
            	(addr = ++text);
            	expression(Lan);
            	(*addr = int((text + 1)));
            	(expr_type = INT);
        }{if ((token == Lan)) {{
              	match(Lan);
              	(*++text = JZ);
              	(addr = ++text);
              	expression(Or);
              	(*addr = int((text + 1)));
              	(expr_type = INT);
          }{if ((token == Or)) {{
                	match(Or);
                	(*++text = PUSH);
                	expression(Xor);
                	(*++text = OR);
                	(expr_type = INT);
            }{if ((token == Xor)) {{
                  	match(Xor);
                  	(*++text = PUSH);
                  	expression(And);
                  	(*++text = XOR);
                  	(expr_type = INT);
              }{if ((token == And)) {{
                    	match(And);
                    	(*++text = PUSH);
                    	expression(Eq);
                    	(*++text = AND);
                    	(expr_type = INT);
                }{if ((token == Eq)) {{
                      	match(Eq);
                      	(*++text = PUSH);
                      	expression(Ne);
                      	(*++text = EQ);
                      	(expr_type = INT);
                  }{if ((token == Ne)) {{
                        	match(Ne);
                        	(*++text = PUSH);
                        	expression(Lt);
                        	(*++text = NE);
                        	(expr_type = INT);
                    }{if ((token == Lt)) {{
                          	match(Lt);
                          	(*++text = PUSH);
                          	expression(Shl);
                          	(*++text = LT);
                          	(expr_type = INT);
                      }{if ((token == Gt)) {{
                            	match(Gt);
                            	(*++text = PUSH);
                            	expression(Shl);
                            	(*++text = GT);
                            	(expr_type = INT);
                        }{if ((token == Le)) {{
                              	match(Le);
                              	(*++text = PUSH);
                              	expression(Shl);
                              	(*++text = LE);
                              	(expr_type = INT);
                          }{if ((token == Ge)) {{
                                	match(Ge);
                                	(*++text = PUSH);
                                	expression(Shl);
                                	(*++text = GE);
                                	(expr_type = INT);
                            }{if ((token == Shl)) {{
                                  	match(Shl);
                                  	(*++text = PUSH);
                                  	expression(Add);
                                  	(*++text = SHL);
                                  	(expr_type = INT);
                              }{if ((token == Shr)) {{
                                    	match(Shr);
                                    	(*++text = PUSH);
                                    	expression(Add);
                                    	(*++text = SHR);
                                    	(expr_type = INT);
                                }{if ((token == Add)) {{
                                      	match(Add);
                                      	(*++text = PUSH);
                                      	expression(Mul);
                                      	(expr_type = tmp);
                                      	if ((expr_type > PTR)) {{
                                            	(*++text = PUSH);
                                            	(*++text = IMM);
                                            	(*++text = sizeof(int));
                                            	(*++text = MUL);
                                        }
                                      	(*++text = ADD);
                                  }{if ((token == Sub)) {{
                                        	match(Sub);
                                        	(*++text = PUSH);
                                        	expression(Mul);
                                        	if (((tmp > PTR) && (tmp == expr_type))) {{
                                              	(*++text = SUB);
                                              	(*++text = PUSH);
                                              	(*++text = IMM);
                                              	(*++text = sizeof(int));
                                              	(*++text = DIV);
                                              	(expr_type = INT);
                                          }{if ((tmp > PTR)) {{
                                                	(*++text = PUSH);
                                                	(*++text = IMM);
                                                	(*++text = sizeof(int));
                                                	(*++text = MUL);
                                                	(*++text = SUB);
                                                	(expr_type = tmp);
                                            }{{
                                                	(*++text = SUB);
                                                	(expr_type = tmp);
                                            }}}
                                    }{if ((token == Mul)) {{
                                          	match(Mul);
                                          	(*++text = PUSH);
                                          	expression(Inc);
                                          	(*++text = MUL);
                                          	(expr_type = tmp);
                                      }{if ((token == Div)) {{
                                            	match(Div);
                                            	(*++text = PUSH);
                                            	expression(Inc);
                                            	(*++text = DIV);
                                            	(expr_type = tmp);
                                        }{if ((token == Mod)) {{
                                              	match(Mod);
                                              	(*++text = PUSH);
                                              	expression(Inc);
                                              	(*++text = MOD);
                                              	(expr_type = tmp);
                                          }{if (((token == Inc) || (token == Dec))) {{
                                                	if ((*text == LI)) {{
                                                      	(*text = PUSH);
                                                      	(*++text = LI);
                                                  }{if ((*text == LC)) {{
                                                        	(*text = PUSH);
                                                        	(*++text = LC);
                                                    }{{
                                                        	printf("%d: bad value in increment
", line);
                                                        	exit(-1);
                                                    }}}
                                                	(*++text = PUSH);
                                                	(*++text = IMM);
                                                	(*++text = (expr_type > PTR)?sizeof(int):sizeof(char));
                                                	(*++text = (token == Inc)?ADD:SUB);
                                                	(*++text = (expr_type == CHAR)?SC:SI);
                                                	(*++text = PUSH);
                                                	(*++text = IMM);
                                                	(*++text = (expr_type > PTR)?sizeof(int):sizeof(char));
                                                	(*++text = (token == Inc)?SUB:ADD);
                                                	match(token);
                                            }{if ((token == Brak)) {{
                                                  	match(Brak);
                                                  	(*++text = PUSH);
                                                  	expression(Assign);
                                                  	match(']');
                                                  	if ((tmp > PTR)) {{
                                                        	(*++text = PUSH);
                                                        	(*++text = IMM);
                                                        	(*++text = sizeof(int));
                                                        	(*++text = MUL);
                                                    }{if ((tmp <= PTR)) {{
                                                          	printf("%d: pointer type expected
", line);
                                                          	exit(-1);
                                                      }}
                                                  	(expr_type = (tmp - PTR));
                                                  	(*++text = ADD);
                                                  	(*++text = (expr_type == CHAR)?LC:LI);
                                              }{{
                                                  	printf("%d: compiler error, token = %d
", line, token);
                                                  	exit(-1);
                                              }}}}}}}}}}}}}}}}}}}}}}}
}}
    }
}
Function statement :
void (){
    	a: (int*), b: (int*)
    	if ((token == If)) {{
          	match(If);
          	match('(');
          	expression(Assign);
          	match(')');
          	(*++text = JZ);
          	(b = ++text);
          	statement();
          	if ((token == Else)) {{
                	match(Else);
                	(*b = int((text + 3)));
                	(*++text = JMP);
                	(b = ++text);
                	statement();
            }
          	(*b = int((text + 1)));
      }{if ((token == While)) {{
            	match(While);
            	(a = (text + 1));
            	match('(');
            	expression(Assign);
            	match(')');
            	(*++text = JZ);
            	(b = ++text);
            	statement();
            	(*++text = JMP);
            	(*++text = int(a));
            	(*b = int((text + 1)));
        }{if ((token == '{')) {{
              	match('{');
              	while ((token != '}')) {{
  	statement();
}}
              	match('}');
          }{if ((token == Return)) {{
                	match(Return);
                	if ((token != ';')) {{
                      	expression(Assign);
                  }
                	match(';');
                	(*++text = LEV);
            }{if ((token == ';')) {{
                  	match(';');
              }{{
                  	expression(Assign);
                  	match(';');
              }}}}}}
}
Function enum_declaration :
void (){
    	i: int
    	(i = 0);
    	while ((token != '}')) {{
  	if ((token != Id)) {{
        	printf("%d: bad enum identifier %d
", line, token);
        	exit(-1);
    }
  	next();
  	if ((token == Assign)) {{
        	next();
        	if ((token != Num)) {{
              	printf("%d: bad enum initializer
", line);
              	exit(-1);
          }
        	(i = token_val);
        	next();
    }
  	(current_id[Class] = Num);
  	(current_id[Type] = INT);
  	(current_id[Value] = i++);
  	if ((token == ',')) {{
        	next();
    }
}}
}
Function function_parameter :
void (){
    	type: int
    	params: int
    	(params = 0);
    	while ((token != ')')) {{
  	(type = INT);
  	if ((token == Int)) {{
        	match(Int);
    }{if ((token == Char)) {{
          	(type = CHAR);
          	match(Char);
      }}
  	while ((token == Mul)) {{
  	match(Mul);
  	(type = (type + PTR));
}}
  	if ((token != Id)) {{
        	printf("%d: bad parameter declaration
", line);
        	exit(-1);
    }
  	if ((current_id[Class] == Loc)) {{
        	printf("%d: duplicate parameter declaration
", line);
        	exit(-1);
    }
  	match(Id);
  	(current_id[BClass] = current_id[Class]);
  	(current_id[Class] = Loc);
  	(current_id[BType] = current_id[Type]);
  	(current_id[Type] = type);
  	(current_id[BValue] = current_id[Value]);
  	(current_id[Value] = params++);
  	if ((token == ',')) {{
        	match(',');
    }
}}
    	(index_of_bp = (params + 1));
}
Function function_body :
void (){
    	pos_local: int
    	type: int
    	(pos_local = index_of_bp);
    	while (((token == Int) || (token == Char))) {{
  	(basetype = (token == Int)?INT:CHAR);
  	match(token);
  	while ((token != ';')) {{
  	(type = basetype);
  	while ((token == Mul)) {{
  	match(Mul);
  	(type = (type + PTR));
}}
  	if ((token != Id)) {{
        	printf("%d: bad local declaration
", line);
        	exit(-1);
    }
  	if ((current_id[Class] == Loc)) {{
        	printf("%d: duplicate local declaration
", line);
        	exit(-1);
    }
  	match(Id);
  	(current_id[BClass] = current_id[Class]);
  	(current_id[Class] = Loc);
  	(current_id[BType] = current_id[Type]);
  	(current_id[Type] = type);
  	(current_id[BValue] = current_id[Value]);
  	(current_id[Value] = ++pos_local);
  	if ((token == ',')) {{
        	match(',');
    }
}}
  	match(';');
}}
    	(*++text = ENT);
    	(*++text = (pos_local - index_of_bp));
    	while ((token != '}')) {{
  	statement();
}}
    	(*++text = LEV);
}
Function function_declaration :
void (){
    	match('(');
    	function_parameter();
    	match(')');
    	match('{');
    	function_body();
    	(current_id = symbols);
    	while (current_id[Token]) {{
  	if ((current_id[Class] == Loc)) {{
        	(current_id[Class] = current_id[BClass]);
        	(current_id[Type] = current_id[BType]);
        	(current_id[Value] = current_id[BValue]);
    }
  	(current_id = (current_id + IdSize));
}}
}
Function global_declaration :
void (){
    	type: int
    	i: int
    	(basetype = INT);
    	if ((token == Enum)) {{
          	match(Enum);
          	if ((token != '{')) {{
                	match(Id);
            }
          	if ((token == '{')) {{
                	match('{');
                	enum_declaration();
                	match('}');
            }
          	match(';');
          	return;
      }
    	if ((token == Int)) {{
          	match(Int);
      }{if ((token == Char)) {{
            	match(Char);
            	(basetype = CHAR);
        }}
    	while (((token != ';') && (token != '}'))) {{
  	(type = basetype);
  	while ((token == Mul)) {{
  	match(Mul);
  	(type = (type + PTR));
}}
  	if ((token != Id)) {{
        	printf("%d: bad global declaration
", line);
        	exit(-1);
    }
  	if (current_id[Class]) {{
        	printf("%d: duplicate global declaration
", line);
        	exit(-1);
    }
  	match(Id);
  	(current_id[Type] = type);
  	if ((token == '(')) {{
        	(current_id[Class] = Fun);
        	(current_id[Value] = int((text + 1)));
        	function_declaration();
    }{{
        	(current_id[Class] = Glo);
        	(current_id[Value] = int(data));
        	(data = (data + sizeof(int)));
    }}
  	if ((token == ',')) {{
        	match(',');
    }
}}
    	next();
}
Function program :
void (){
    	next();
    	while ((token > 0)) {{
  	global_declaration();
}}
}
Function eval :
int (){
    	op: int, tmp: (int*)
    	(cycle = 0);
    	while (1) {{
  	cycle++;
  	(op = *pc++);
  	if (debug) {{
        	printf("%d> %.4s", cycle, &"LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[(op * 5)]);
        	if ((op <= ADJ)) {printf(" %d
", *pc);{printf("
");}
    }
  	if ((op == IMM)) {{
        	(ax = *pc++);
    }{if ((op == LC)) {{
          	(ax = *(char*)(ax));
      }{if ((op == LI)) {{
            	(ax = *(int*)(ax));
        }{if ((op == SC)) {{
              	(ax = (*(char*)(*sp++) = ax));
          }{if ((op == SI)) {{
                	(*(int*)(*sp++) = ax);
            }{if ((op == PUSH)) {{
                  	(*--sp = ax);
              }{if ((op == JMP)) {{
                    	(pc = (int*)(*pc));
                }{if ((op == JZ)) {{
                      	(pc = ax?(pc + 1):(int*)(*pc));
                  }{if ((op == JNZ)) {{
                        	(pc = ax?(int*)(*pc):(pc + 1));
                    }{if ((op == CALL)) {{
                          	(*--sp = int((pc + 1)));
                          	(pc = (int*)(*pc));
                      }{if ((op == ENT)) {{
                            	(*--sp = int(bp));
                            	(bp = sp);
                            	(sp = (sp - *pc++));
                        }{if ((op == ADJ)) {{
                              	(sp = (sp + *pc++));
                          }{if ((op == LEV)) {{
                                	(sp = bp);
                                	(bp = (int*)(*sp++));
                                	(pc = (int*)(*sp++));
                            }{if ((op == LEA)) {{
                                  	(ax = int((bp + *pc++)));
                              }{if ((op == OR)) {(ax = (*sp++ | ax));{if ((op == XOR)) {(ax = (*sp++ ^ ax));{if ((op == AND)) {(ax = (*sp++ & ax));{if ((op == EQ)) {(ax = (*sp++ == ax));{if ((op == NE)) {(ax = (*sp++ != ax));{if ((op == LT)) {(ax = (*sp++ <= ax));{if ((op == LE)) {(ax = (*sp++ <= ax));{if ((op == GT)) {(ax = (*sp++ > ax));{if ((op == GE)) {(ax = (*sp++ >= ax));{if ((op == SHL)) {(ax = (*sp++ << ax));{if ((op == SHR)) {(ax = (*sp++ >> ax));{if ((op == ADD)) {(ax = (*sp++ + ax));{if ((op == SUB)) {(ax = (*sp++ - ax));{if ((op == MUL)) {(ax = (*sp++ * ax));{if ((op == DIV)) {(ax = (*sp++ / ax));{if ((op == MOD)) {(ax = (*sp++ % ax));{if ((op == EXIT)) {{
                                                                    	printf("exit(%d)", *sp);
                                                                    	return *sp;
                                                                }{if ((op == OPEN)) {{
                                                                      	(ax = open((char*)(sp[1]), sp[0]));
                                                                  }{if ((op == CLOS)) {{
                                                                        	(ax = close(*sp));
                                                                    }{if ((op == READ)) {{
                                                                          	(ax = read(sp[2], (char*)(sp[1]), *sp));
                                                                      }{if ((op == PRTF)) {{
                                                                            	(tmp = (sp + pc[1]));
                                                                            	(ax = printf((char*)(tmp[-1]), tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]));
                                                                        }{if ((op == MALC)) {{
                                                                              	(ax = int(malloc(*sp)));
                                                                          }{if ((op == MSET)) {{
                                                                                	(ax = int(memset((char*)(sp[2]), sp[1], *sp)));
                                                                            }{if ((op == MCMP)) {{
                                                                                  	(ax = memcmp((char*)(sp[2]), (char*)(sp[1]), *sp));
                                                                              }{{
                                                                                  	printf("unknown instruction:%d
", op);
                                                                                  	return -1;
                                                                              }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
}}
}
Function main :
int (int argc, ((char*)*) argv){
    	i: int, fd: int
    	tmp: (int*)
    	argc--;
    	argv++;
    	if ((((argc > 0) && (**argv == '-')) && (*argv[1] == 's'))) {{
          	(assembly = 1);
          	--argc;
          	++argv;
      }
    	if ((((argc > 0) && (**argv == '-')) && (*argv[1] == 'd'))) {{
          	(debug = 1);
          	--argc;
          	++argv;
      }
    	if ((argc <= 1)) {{
          	printf("usage: xc [-s] [-d] file ...
");
          	return -1;
      }
    	if (((fd = open(*argv, 0)) <= 0)) {{
          	printf("could not open(%s)
", *argv);
          	return -1;
      }
    	(poolsize = (256 * 1024));
    	(line = 1);
    	if (!(text = malloc(poolsize))) {{
          	printf("could not malloc(%d) for text area
", poolsize);
          	return -1;
      }
    	if (!(data = malloc(poolsize))) {{
          	printf("could not malloc(%d) for data area
", poolsize);
          	return -1;
      }
    	if (!(stack = malloc(poolsize))) {{
          	printf("could not malloc(%d) for stack area
", poolsize);
          	return -1;
      }
    	if (!(symbols = malloc(poolsize))) {{
          	printf("could not malloc(%d) for symbol table
", poolsize);
          	return -1;
      }
    	memset(text, 0, poolsize);
    	memset(data, 0, poolsize);
    	memset(stack, 0, poolsize);
    	memset(symbols, 0, poolsize);
    	(old_text = text);
    	(src = "char else enum if int return sizeof while open read close printf malloc memset memcmp exit void main");
    	(i = Char);
    	while ((i <= While)) {{
  	next();
  	(current_id[Token] = i++);
}}
    	(i = OPEN);
    	while ((i <= EXIT)) {{
  	next();
  	(current_id[Class] = Sys);
  	(current_id[Type] = INT);
  	(current_id[Value] = i++);
}}
    	next();
    	(current_id[Token] = Char);
    	next();
    	(idmain = current_id);
    	if (!(src = (old_src = malloc(poolsize)))) {{
          	printf("could not malloc(%d) for source area
", poolsize);
          	return -1;
      }
    	if (((i = read(fd, src, (poolsize - 1))) <= 0)) {{
          	printf("read() returned %d
", i);
          	return -1;
      }
    	(src[i] = 0);
    	close(fd);
    	program();
    	if (!(pc = (int*)(idmain[Value]))) {{
          	printf("main() not defined
");
          	return -1;
      }
    	if (assembly) {{
          	return 0;
      }
    	(sp = (int*)((int(stack) + poolsize)));
    	(*--sp = EXIT);
    	(*--sp = PUSH);
    	(tmp = sp);
    	(*--sp = argc);
    	(*--sp = int(argv));
    	(*--sp = int(tmp));
    	return eval();
}
