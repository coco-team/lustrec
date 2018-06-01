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
				body = [];
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
  
