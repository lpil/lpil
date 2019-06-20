import { connect } from "react-redux";
import CellGrid from "../components/cell_grid";

const mapStateToProps = (state) => {
  return {
    rowStates: state.grid,
  };
};

const mapDispatchToProps = () => {
  return {
  };
};

const Grid = connect(mapStateToProps, mapDispatchToProps)(CellGrid);

export default Grid;
