test_array_1 <- xml2::read_xml('
<void property="testProperty1">
  <array class="int" length="3">
    <void index="0">
      <int>6</int>
    </void>
    <void index="1">
      <int>7</int>
    </void>
    <void index="2">
      <int>8</int>
    </void>
  </array>
</void>
')

test_array_2 <- xml2::read_xml('
<void property="testProperty1">
  <array class="int" length="5">
    <void index="1">
      <int>6</int>
    </void>
    <void index="2">
      <int>7</int>
    </void>
    <void index="4">
      <int>8</int>
    </void>
  </array>
</void>
')

string_array <- xml2::read_xml('
<void property="testProperty1">
  <array class="java.lang.String" length="3">
    <void index="0">
      <string>Gaborone (%)</string>
    </void>
    <void index="1">
      <string>Francistown (%)</string>
    </void>
  </array>
</void>
')

double_array <- xml2::read_xml('
<void property="testProperty1">
  <array class="double" length="3">
    <void index="0">
      <double>6.2</double>
    </void>
    <void index="1">
      <double>7.000005</double>
    </void>
  </array>
</void>
')

logical_array <- xml2::read_xml('
<void property="testProperty1">
  <array class="boolean" length="3">
    <void index="0">
      <boolean>true</boolean>
    </void>
    <void index="1">
      <boolean>false</boolean>
    </void>
  </array>
</void>
')

test_ragged_matrix <- xml2::read_xml('
<void property="testProperty">
  <array class="[D" length="3">
    <void index="0">
      <array class="int" length="3">
        <void index="0">
          <int>6</int>
        </void>
        <void index="1">
          <int>7</int>
        </void>
        <void index="2">
          <int>8</int>
        </void>
      </array>
    </void>
    <void index="2">
      <array class="int" length="5">
        <void index="1">
          <int>6</int>
        </void>
        <void index="2">
          <int>7</int>
        </void>
        <void index="4">
          <int>8</int>
        </void>
      </array>
    </void>
  </array>
</void>
')

test_matrix <- xml2::read_xml('
<void property="testProperty">
  <array class="[D" length="3">
    <void index="0">
      <array class="int" length="5">
        <void index="0">
          <int>6</int>
        </void>
        <void index="1">
          <int>7</int>
        </void>
        <void index="2">
          <int>8</int>
        </void>
      </array>
    </void>
    <void index="2">
      <array class="int" length="5">
        <void index="1">
          <int>6</int>
        </void>
        <void index="2">
          <int>7</int>
        </void>
        <void index="4">
          <int>8</int>
        </void>
      </array>
    </void>
  </array>
</void>
')

test_nodeset <- xml2::read_xml('
<object class="epp2011.core.sets.Workset" id="Workset0">
  <void property="testProperty1">
    <array class="int" length="3">
      <void index="0">
        <int>6</int>
      </void>
      <void index="1">
        <int>-1</int>
      </void>
      <void index="2">
        <int>8</int>
      </void>
    </array>
  </void>
  <void property="testProperty2">
    <array class="[D" length="3">
      <void index="0">
        <array class="int" length="5">
          <void index="0">
            <int>6</int>
          </void>
          <void index="1">
            <int>7</int>
          </void>
          <void index="2">
             <int>8</int>
          </void>
        </array>
      </void>
      <void index="2">
        <array class="int" length="5">
          <void index="1">
            <int>6</int>
          </void>
          <void index="2">
            <int>7</int>
          </void>
          <void index="4">
            <int>-1</int>
          </void>
        </array>
      </void>
    </array>
  </void>
  <void property="testProperty3">
    <int>72</int>
  </void>
  <void property="testProperty4">
    <double>0.1</double>
  </void>
  <void property="testProperty5">
    <string>Example</string>
  </void>
  <void property="testProperty6">
    <boolean>true</boolean>
  </void>
  <void property="testProperty7">
    <object class="java.lang.Enum" method="valueOf">
      <class>epp2011.EPPConstants$LocationType</class>
      <string>URBAN</string>
    </object>
  </void>
</object>
')

test_field_node <- xml2::read_xml('
<void method="add">
  <object class="epp2011.core.sets.SurveyData" id="SurveyData0">
    <void class="epp2011.core.sets.SurveyData" method="getField">
      <string>name</string>
      <void method="set">
        <object idref="SurveyData0"/>
        <string>2004 BAIS</string>
      </void>
    </void>
    <void class="epp2011.core.sets.SurveyData" method="getField">
      <string>year</string>
      <void method="set">
        <object idref="SurveyData0"/>
        <int>2004</int>
      </void>
    </void>
    <void id="SurveyData$AggregateData0" property="aggregateData">
      <void class="epp2011.core.sets.SurveyData" method="getField">
        <string>surveyHIV</string>
        <void method="set">
          <object idref="SurveyData$AggregateData0"/>
          <double>24.68</double>
        </void>
      </void>
      <void class="epp2011.core.sets.SurveyData" method="getField">
        <string>surveyStandardError</string>
        <void method="set">
          <object idref="SurveyData$AggregateData0"/>
          <double>0.74</double>
        </void>
      </void>
    </void>
  </object>
</void>
')

