/**
 * analogyStrings function
 * Brad Rogers
 * plugin for generating a stimulus of two sampled strings in an HTML table
 **/

jsPsych.plugins['analogy-strings'] = (function(nStims, blockSize, nBlocks, nA, nB){
  var sampleSizes = [nA,nB];
  var stringLength = blockSize*nBlocks;

  seqBlock =[];
  for (n=0; n<blockSize; n++){
    seqBlock[n] = n+1
  };

  function flatten(array){
    return array.reduce(function(arr, elem){
      var items = Array.isArray(elem) ? flatten(elem) : [elem];
      return arr.concat(items);
     },[]);
   };

  var stringConditions=[];
  for (stim=1; stim<=nStims; stim++){
    stringConditions[stim-1]=[Math.random()>0.5,Math.random()>0.5,Math.random()>0.5,Math.random()>0.5]
  };

  var strings=[];
  for (stim=0; stim<nStims; stim++){
    var fullStringA=[];
    var fullStringB=[];
    var blockMap =[];
    var blockA = [];
    var blockB = [];
    var blockBNums = [];

    for (n=0; n<nBlocks; n++){
      if(stringConditions[stim][0]){baseBlock=shuffle(seqBlock) } else {baseBlock=seqBlock };
      if(stringConditions[stim][1]){blockA=   shuffle(baseBlock)} else {blockA=   baseBlock};
      if(stringConditions[stim][2]){baseMap=  shuffle(seqBlock) } else {baseMap=  seqBlock };
      if(stringConditions[stim][3]){blockMap= shuffle(baseMap)  } else {blockMap= baseMap  };

      for (m=0; m<blockSize; m++){
        blockBNums[m] = blockMap[blockA[m]-1];
        blockB[m] = String.fromCharCode(96+blockBNums[m]).toUpperCase();
      };
      fullStringA.push(blockA.toString());
      fullStringB.push(blockB.toString());
    };
    strings.push([fullStringA.toString().replace(/,/gi,''),fullStringB.toString().replace(/,/gi,'')]);
  };

  seqString = [];
  for (n=0; n<stringLength; n++){
    seqString[n] = n
  };

  String.prototype.replaceAt=function(index, character){
    return this.substr(0, index) + character + this.substr(index+character.length);
  };

  var stringsMasked = $.extend(true, [], strings);
  var mask = [];
  for (stim=0; stim<nStims; stim++){
    for (str=0; str<2; str++){
      mask = shuffle(seqString).slice(0,sampleSizes[str]);
      for (i=0; i<sampleSizes[str]; i++){
        stringsMasked[stim][str]=stringsMasked[stim][str].replaceAt(mask[i],' ');
       };
     };
   };

  //* DEFINE BLOCK 1 HTML Stimuli Tables */
  var test_stims = [];
  for (stim=1; stim<=nStims; stim++){
    var rowTitles = ['string A','string B'];
    htmlTable = [];
    htmlTable = document.createElement('table');
    for (var j=1; j<=2; j++){
      string = stringsMasked[stim-1][j-1];
      var tableRow = document.createElement('tr');
      for (var i=0; i<=stringLength; i++){
        var tableDatum = document.createElement('td');
        tableDatum.style.border = '1px solid black';
        tableDatum.style.paddingLeft =  '10px';
        tableDatum.style.paddingRight = '10px';
        tableDatum.style.paddingTop =   '0px';
        tableDatum.style.paddingBottom ='0px';
        tableDatum.style.minWidth = '10px';
        if(i==0){
          var cellText = document.createTextNode(rowTitles[j-1]);
          tableDatum.style.fontStyle ='italic';
        }else{
          var cellText = document.createTextNode(string.charAt(i-1));
          tableDatum.style.fontStyle='normal';
        };
        tableDatum.appendChild(cellText);
        tableRow.appendChild(tableDatum);
      };
      htmlTable.appendChild(tableRow);
    };

    htmlTable.style.border = '2px solid black';
    htmlTable.style.borderSpacing = '0px 1px';
    test_stims[stim-1] = {stimulus: [htmlTable], options: [['A','B','C']], conditions: [stringConditions[stim-1]]};
  };
  return test_stims;
})();
