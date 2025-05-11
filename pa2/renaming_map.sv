// Renaming map module
// While you are free to structure your implementation however you
// like, you are advised to only add code to the TODO sections
module renaming_map import ariane_pkg::*; #(
    parameter int unsigned ARCH_REG_WIDTH = 5,
    parameter int unsigned PHYS_REG_WIDTH = 6
)(
    // Clock and reset signals
    input logic clk_i,
    input logic rst_ni,

    // Indicator that there is a new instruction to rename
    input logic fetch_entry_ready_i,

    // Input decoded instruction entry from the ID stage
    input issue_struct_t issue_n,

    // Output instruction entry with registers renamed
    output issue_struct_t issue_q,

    // Destination register of the committing instruction
    input logic [PHYS_REG_WIDTH-1:0] waddr_i,
    
    // Indicator signal that there is a new committing instruction
    input logic we_gp_i
);

    // 32 architectural registers and 64 physical registers
    localparam ARCH_NUM_REGS = 2**ARCH_REG_WIDTH;
    localparam PHYS_NUM_REGS = 2**PHYS_REG_WIDTH;

    logic [PHYS_REG_WIDTH-1:0] rs1;
    logic [PHYS_REG_WIDTH-1:0] rs2;
    logic [PHYS_REG_WIDTH-1:0] rd;

    // TODO: ADD STRUCTURES TO EXECUTE REGISTER RENAMING
    logic [PHYS_NUM_REGS-1:0] alloc; //indicate whether physical register [i] is allocated
    logic [PHYS_NUM_REGS-1:0] dealloc; // indicate whether physical register [i] is to be deallocated when inst commits 
    logic [ARCH_NUM_REGS-1:0][PHYS_REG_WIDTH-1:0] map; //map table from arf to rrf
    logic [PHYS_NUM_REGS-1:0][PHYS_REG_WIDTH-1:0] map_dealloc; //dealloc map from rrf to rrf
    logic [PHYS_REG_WIDTH-1:0] rs1_phy;
    logic [PHYS_REG_WIDTH-1:0] rs2_phy;
    logic [PHYS_REG_WIDTH-1:0] rd_phy;
    logic [PHYS_REG_WIDTH-1:0] rd_phy_prev;

    // Positive clock edge used for renaming new instructions
    always @(posedge clk_i, negedge rst_ni) begin
        // Processor reset: revert renaming state to reset conditions    
        if (~rst_ni) begin

            // TODO: ADD LOGIC TO RESET RENAMING STATE
	    // map table
	    alloc 	= '0;
	    alloc[0]	= 1'b1; //prf0 always allocated
	    dealloc	= '0;
	    map 	= '0;
	    map_dealloc = '0;
            // reg
	    rs1		= '0;
	    rs2		= '0;
	    rd		= '0;
	    rs1_phy	= '0;
	    rs2_phy	= '0;
	    rd_phy	= '0;
	    rd_phy_prev = '0;
        // New incoming valid instruction to rename   
        end else if (fetch_entry_ready_i && issue_n.valid) begin
            // Get values of registers in new instruction
            rs1 = issue_n.sbe.rs1[PHYS_REG_WIDTH-1:0];
            rs2 = issue_n.sbe.rs2[PHYS_REG_WIDTH-1:0];
            rd = issue_n.sbe.rd[PHYS_REG_WIDTH-1:0];

            // Set outgoing instruction to incoming instruction without
            // renaming by default. Keep this line since all fields of the 
            // incoming issue_struct_t should carry over to the output
            // except for the register values, which you may rename below
            issue_q = issue_n;

            // TODO: ADD LOGIC TO RENAME OUTGOING INSTRUCTION
            // The registers of the outgoing instruction issue_q can be set like so:
	    rs1_phy = map[rs1];
	    rs2_phy = map[rs2];
	    if (rd == '0) begin
	        rd_phy = '0;
	    end else begin
	        for (int i=1; i<ARCH_NUM_REGS; i++) begin
		    // find lowest index free pfr to map to arf
		    if (~alloc[i]) begin
		        rd_phy = i;
			alloc[i] = 1'b1;
			break;
		    end
		end
		rd_phy_prev = map[rd];
		map[rd] = rd_phy;
	    	if (rd_phy_prev != '0) begin
		    //arf already mapped to other rrf, set to deallocate
		    dealloc[rd_phy] = 1'b1;
		    map_dealloc[rd_phy] = rd_phy_prev;
		end
	    end

            issue_q.sbe.rs1[PHYS_REG_WIDTH-1:0] = rs1_phy;
            issue_q.sbe.rs2[PHYS_REG_WIDTH-1:0] = rs2_phy;
            issue_q.sbe.rd[PHYS_REG_WIDTH-1:0]  = rd_phy;

    
        // If there is no new instruction this clock cycle, simply pass on the
        // incoming instruction without renaming
        end else begin
            issue_q = issue_n;
        end
    end
    

    // Negative clock edge used for physical register deallocation 
    always @(negedge clk_i) begin
        if (rst_ni) begin
            // If there is a new committing instruction and its prd is not pr0,
            // execute register deallocation logic to reuse physical registers
            if (we_gp_i && waddr_i != 0) begin
        
                // TODO: IMPLEMENT REGISTER DEALLOCATION LOGIC    
		if (dealloc[waddr_i] != 1'b0) begin
		    rd_phy_prev		= map_dealloc[waddr_i];
		    alloc[rd_phy_prev]	= 1'b0;
		    dealloc[waddr_i]	= 1'b0;
		    map_dealloc[waddr_i]= '0;
	    	end

            end
        end
    end
endmodule
