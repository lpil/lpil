import { connect } from "react-redux";
import BPM from "../components/bpm";

const mapStateToProps = (state) => {
  return {
    currentBPM: state.bpm,
  };
};

const mapDispatchToProps = () => {
  return {
  };
};

const MetaBPM = connect(mapStateToProps, mapDispatchToProps)(BPM);

export default MetaBPM;
