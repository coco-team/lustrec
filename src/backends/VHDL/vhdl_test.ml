open Vhdl_ast

let design1 = {
    packages = [{name = "typedef"; shared_defs = [Subtype{name = "byte"; definition = Bit_vector (7, 0)}]}];
    libraries = [Use ["work";"typedef";"all"]];
    entities = [{ name = "data_path";
		  generics = [];
		  ports = [
		      {name = "clk"; kind = InPort; typ = Base "boolean"};
		      {name = "rst"; kind = InPort; typ = Base "boolean"};
		      {name = "s_1"; kind = InPort; typ = Base "boolean"};
		      {name = "s0"; kind = InPort; typ = Base "bit"};
		      {name = "s1"; kind = InPort; typ = Base "bit"};
		      {name = "d0"; kind = InPort; typ = Base "byte"};
		      {name = "d1"; kind = InPort; typ = Base "byte"};
		      {name = "d2"; kind = InPort; typ = Base "byte"};
		      {name = "d3"; kind = InPort; typ = Base "byte"};
		      {name = "q"; kind = OutPort; typ = Base "byte"};
		      
		    ];
		}];
    architectures = [{
			name = "behavior";
			entity = "data_path";
			declarations = [
			    SigDecl { name = "reg"; typ = Base "byte"; init_val = None};
			    SigDecl { name = "shft"; typ = Base "byte"; init_val = None};
			    SigDecl { name = "sel"; typ = Bit_vector(1,0); init_val = None};

			  ];
			body = [
			    Process {
				id = None;
				active_sigs = ["clk"; "rst"];
				body = [
				    If {
					if_cases = [
					  {
					    if_cond = Sig{ name = "rst"; att = None };
					    if_block = [
						SigSeqAssign { lhs = "req"; rhs = Cst (CstBV("x", "00"))};
						SigSeqAssign { lhs = "shft"; rhs = Cst (CstBV("x", "00"))};
					      ];
					  };
					  {
					    if_cond = Op {id = "and"; args = [Sig{ name = "clk"; att = None };
									      Sig{ name = "clk"; att = Some (SigAtt "event") }]};
					    if_block = [
						SigSeqAssign { lhs = "req"; rhs = Op { id = "&"; args = [
											   Sig{ name = "s0"; att = None };
											   Sig{ name = "s1"; att = None }
											 ]
										     }
							     };
						Case {
						    guard = Sig{ name = "sel"; att = None };
						    branches = [
							{
							  when_cond = Cst (CstBV("b", "00"));
							  when_stmt = SigSeqAssign { lhs = "req"; rhs = Sig{ name = "d0"; att = None }};
							};
							{
							  when_cond = Cst (CstBV("b", "10"));
							  when_stmt = SigSeqAssign { lhs = "req"; rhs = Sig{ name = "d1"; att = None }};
							};
							{
							  when_cond = Cst (CstBV("b", "01"));
							  when_stmt = SigSeqAssign { lhs = "req"; rhs = Sig{ name = "d2"; att = None }};
							};
							{
							  when_cond = Cst (CstBV("b", "11"));
							  when_stmt = SigSeqAssign { lhs = "req"; rhs = Sig{ name = "d3"; att = None }};
							};

						      ]

						  };
						If {
						    if_cases = [
						      {
							if_cond = Sig{ name = "s_1"; att = None };
							if_block = [
							    SigSeqAssign {
								lhs = "shft";
								rhs = Op { id = "&";
									   args = [
									       SuffixMod {
										   expr = Sig{ name = "shft"; att = None };
										   selection = Range (6,0);
										 };
									       SuffixMod {
										   expr = Sig{ name = "shft"; att = None };
										   selection = Idx 7;
										 }
									     ]
									 }
							      };
							  ];
						      };
						    ];
						    default = Some [
								  SigSeqAssign { lhs = "shft"; rhs = Var "reg"};
								]
						  };
					      ];
					  };
					  
					];
					default = None;
				      }
				  ];
			      };
			    SigAssign {
				lhs = "q";
				rhs = Var "shft";
				cond = None;
			      }

			  ];
		      }];
    configuration = None;
  }
  
