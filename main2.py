import sys
import json
from PyQt5.QtWidgets import QApplication, QWidget, QVBoxLayout, QComboBox, QLabel, QPushButton, QTableWidget, QTableWidgetItem
from PyQt5.QtCore import Qt


coefficients = {
  "Model_1": [32.723, -3.0881],
  "Model_2": [45.9819, -3.2267, -1.9094],
  "Model_3": [47.2069, -2.2679, -1.7423, -0.1929],
  "Model_4": [46.1805, -3.5243, -1.5931, 0.4979, -0.8616],
  "Model_5": [47.9767, -2.8175, -1.6881, 0.4646, -0.719, -0.1839],
  "Model_6": [47.5036, -2.5325, -1.2078, -1.3832, 0.4864, -0.541, -0.1798],
  "Model_7": [46.8555, 0.9191, -2.9113, -1.93, -1.6048, 0.771, -0.4339, -0.2065],
  "Model_8": [45.4315, 1.6297, -2.76, -2.0731, -1.378, 0.5857, -1.1206, 0.7695, -0.2144],
  "Model_9": [45.3145, 1.6712, -2.7937, -2.2885, -1.2925, 0.5965, -1.0125, 1.0274, -0.3748, -0.2226],
  "Model_10": [39.9801, 1.3405, -1.8449, -1.8819, -1.4911, -1.2313, 0.9369, -0.4322, -0.5483, -0.1423, 1.3709],
  "Model_11": [38.4818, 1.7365, -2.645, -2.0646, -1.3071, 0.4353, -1.0014, 0.2846, 0.9924, -0.4698, -0.2093, 1.3316],
  "Model_12": [38.8809, 0.794, 1.2415, -2.7232, -2.1336, -1.3216, 0.5142, -1.1424, 0.3381, 0.9771, -0.4765, -0.2246, 1.3178],
  "Model_13": [38.444, 0.8513, 1.1142, -2.834, -1.8352, -1.1614, 0.471, -1.0229, 0.3436, 0.9565, -0.4317, -0.3804, -0.2114, 1.3991],
  "Model_14": [39.2591, 0.8342, 1.1054, -2.355, -0.7172, -1.7525, -1.1973, 0.3782, -0.8713, 0.3289, 0.938, -0.4246, -0.4092, -0.1998, 1.3381],
  "Model_15": [39.2002, 0.8707, 1.0124, -2.4014, -0.6831, -0.4807, -1.6295, -1.1787, 0.4548, -0.8961, 0.3411, 0.9449, -0.4451, -0.3948, -0.1985, 1.3623],
  "Model_16": [39.4747, 0.8618, 0.92, -2.3356, -0.8508, -0.6374, -1.5552, -1.1679, 0.4295, -0.7573, 0.3208, 0.9466, -0.4833, -0.4206, -0.0599, -0.1496, 1.4156],
  "Model_17": [39.5791, 0.89, 0.8216, -2.3694, -0.8557, -0.7758, -1.5073, -1.1534, 0.4902, -0.7291, 0.2961, 0.9345, -0.4822, -0.1688, -0.3619, -0.0691, -0.1412, 1.4203],
  "Model_18": [39.7809, 0.848, 0.8594, -2.3878, -0.8275, -0.7345, -1.5098, -1.1532, 0.4677, -0.7408, 0.0979, 0.2405, 0.9129, -0.4636, -0.1619, -0.3718, -0.0717, -0.1383, 1.4002],
  "Model_19": [39.0455, 0.864, 0.8469, -2.375, -0.8413, -0.7414, -1.5054, -1.1481, 0.4574, -0.7261, 0.1149, 0.2245, 0.9088, -0.4645, -0.171, -0.3744, -0.0643, -0.1361, 0.2875, 1.2125],
  "Model_20": [39.8697, 0.8692, 0.7328, -2.3724, -0.8285, -0.9282, -1.4905, -1.0316, -0.0972, 0.5516, -0.7234, 0.0854, 0.2175, 0.9141, -0.4748, -0.1701, -0.2892, -0.1267, -0.0809, -0.1358, 1.4168],
  "Model_21": [39.2296, 0.8803, 0.7331, -2.365, -0.8387, -0.9183, -1.4894, -1.0357, -0.0919, 0.5367, -0.7116, 0.1004, 0.2055, 0.9105, -0.475, -0.1784, -0.3006, -0.1122, -0.074, -0.1339, 0.2433, 1.2584],
  "Model_22": [39.231, 0.8799, 0.7332, -2.3631, -0.8407, -0.9161, -1.4875, -1.0365, -0.0913, 0.5354, -0.7114, 0.1007, 0.2058, 0.9104, -0.4747, -0.1762, -0.0037, -0.3003, -0.1114, -0.0739, -0.1339, 0.2437, 1.258]
}

variables = {
  "Model_1": ["(Intercept)", "InternalPoplitealSciaticMotorRight_Ankle_CFPI"],
  "Model_2": ["(Intercept)", "UlnarMotorLeft_Wrist_FingerAdduction", "InternalPoplitealSciaticMotorRight_Ankle_CFPI"],
  "Model_3": ["(Intercept)", "UlnarMotorLeft_Wrist_FingerAdduction", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "MedianVelocityLeft"],
  "Model_4": ["(Intercept)", "UlnarMotorLeft_Wrist_FingerAdduction", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "UlnarSensoryRight", "SuralSensitifD"],
  "Model_5": ["(Intercept)", "UlnarMotorLeft_Wrist_FingerAdduction", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "UlnarSensoryRight", "MusculocutaneousSensoryLeft", "MedianVelocityLeft"],
  "Model_6": ["(Intercept)", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "UlnarSensoryRight", "SuralSensoryLeft", "MedianVelocityLeft"],
  "Model_7": ["(Intercept)", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "UlnarSensoryRight", "UlnarSensoryLeft", "MedianVelocityLeft"],
  "Model_8": ["(Intercept)", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "UlnarSensoryRight", "MedianVelocityLeft"],
  "Model_9": ["(Intercept)", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MedianVelocityLeft"],
  "Model_10": ["(Intercept)", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "UlnarSensoryRight", "UlnarSensoryLeft", "SuralSensitifD", "MedianVelocityRight", "MedianDistalLatencyLeft"],
  "Model_11": ["(Intercept)", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_12": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_13": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "SuralSensitifD", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_14": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "SuralSensitifD", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_15": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "SuralSensitifD", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_16": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "SuralSensitifD", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_17": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "SuralSensitifD", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_18": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryRight", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "SuralSensitifD", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_19": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryRight", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "SuralSensitifD", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyRight", "MedianDistalLatencyLeft"],
  "Model_20": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "InternalPoplitealSciaticMotorLeft_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryRight", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "SuralSensitifD", "SuralSensoryLeft", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyLeft"],
  "Model_21": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "InternalPoplitealSciaticMotorLeft_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryRight", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "SuralSensitifD", "SuralSensoryLeft", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyRight", "MedianDistalLatencyLeft"],
  "Model_22": ["(Intercept)", "MedianMotorRight_Wrist_ThumbAbduction", "MedianMotorLeft_Wrist_ThumbAbduction", "UlnarMotorRight_Wrist_FingerAdduction", "UlnarMotorLeft_Wrist_FingerAdduction", "ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis", "ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis", "InternalPoplitealSciaticMotorRight_Ankle_CFPI", "InternalPoplitealSciaticMotorLeft_Ankle_CFPI", "RadialSensoryRight", "RadialSensoryLeft", "MedianSensoryRight", "MedianSensoryLeft", "UlnarSensoryRight", "UlnarSensoryLeft", "MusculocutaneousSensoryRight", "MusculocutaneousSensoryLeft", "SuralSensitifD", "SuralSensoryLeft", "MedianVelocityRight", "MedianVelocityLeft", "MedianDistalLatencyRight", "MedianDistalLatencyLeft"]
}




class ModelViewer(QWidget):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("Model Coefficients Viewer")
        self.setGeometry(100, 100, 600, 400)

        # Ensure variables are defined
        self.model_data = {'coefficients': coefficients, 'variables': variables}

        # Set up the layout
        self.layout = QVBoxLayout()

        # Combo box for selecting models
        self.model_selector = QComboBox()
        self.update_model_selector()  # Populate dropdown
        self.model_selector.currentIndexChanged.connect(lambda: self.update_model_info())

        # Label to show selected model coefficients
        self.coefficients_label = QLabel("Model Coefficients will appear here.")

        # Table to display variables
        self.variables_table = QTableWidget()
        self.variables_table.setColumnCount(1)
        self.variables_table.setHorizontalHeaderLabels(['Variables'])

        # Add widgets to the layout
        self.layout.addWidget(self.model_selector)
        self.layout.addWidget(self.coefficients_label)
        self.layout.addWidget(self.variables_table)

        self.setLayout(self.layout)

    def update_model_selector(self):
        """ Updates the dropdown with model names after loading JSON files. """
        models = set(self.model_data['coefficients'].keys()) & set(self.model_data['variables'].keys())
        self.model_selector.clear()
        self.model_selector.addItems(models)

    def update_model_info(self):
        """ Updates the displayed coefficients and variables when a model is selected. """
        selected_model = self.model_selector.currentText()
        if not selected_model:  # Ensure selection is valid
            return

        # Update Coefficients
        coef = self.model_data['coefficients'].get(selected_model, [])
        coef_text = ', '.join(map(str, coef))
        self.coefficients_label.setText(f"Coefficients: {coef_text}")

        # Update Variables
        var_list = self.model_data['variables'].get(selected_model, [])
        self.variables_table.setRowCount(len(var_list))

        for row, var in enumerate(var_list):
            item = QTableWidgetItem(var)
            self.variables_table.setItem(row, 0, item)

if __name__ == '__main__':
    app = QApplication(sys.argv)
    
    viewer = ModelViewer()
    viewer.show()

    sys.exit(app.exec_())
