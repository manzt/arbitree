import { reactShinyInput } from 'reactR';
import Scatterplot from 'arbitree-react-component';

const ScatterInput = ({ configuration, value, setValue }) => {
  function handleOnDrawingFinish(points) {
    console.log(points);
    setValue(JSON.stringify(points));
  }
  
  return (
    <Scatterplot
      data={ JSON.parse(value) }
      onDrawingFinish={handleOnDrawingFinish}
    />
  );
};

reactShinyInput('.arbitree', 'arbitree.arbitree', ScatterInput);