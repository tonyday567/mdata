{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dataset management and loading
module DataFrame.Datasets
  ( Dataset (..),
    DatasetType (..),
    SetType (..),
    defaultDatasets,
    getDataset,
    loadDataset,
    classificationNames,
    regressionNames,
    -- Simple dataset registry
    datasets,
    datasetNames,
    lookupUrl,
    localDatasets,
  )
where

import DataFrame qualified as D
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict qualified as Map
import System.Directory (doesDirectoryExist, listDirectory, getHomeDirectory)
import System.FilePath ((</>))
-- import Network.HTTP.Client for future remote dataset support

-- | Classification or regression dataset type
data SetType
  = Classification
  | Regression
  deriving (Show, Generic, Eq, Read)

-- | Dataset source type
data DatasetType
  = Local
  | UCI
  | Kaggle
  | PMLB
  deriving (Show, Eq, Generic)

-- | Dataset metadata
data Dataset = Dataset
  { dsName :: Text,
    dsType :: DatasetType,
    dsPath :: FilePath,
    dsUrl :: Maybe String,
    dsKaggleDataset :: Maybe String,
    dsPMLBSetType :: Maybe String
  }
  deriving (Show, Eq, Generic)

-- | Default built-in datasets
defaultDatasets :: Map.Map Text Dataset
defaultDatasets =
  Map.fromList
    [ ( "penguins",
        Dataset
          { dsName = "penguins",
            dsType = Local,
            dsPath = "penguins.csv",
            dsUrl = Nothing,
            dsKaggleDataset = Nothing,
            dsPMLBSetType = Nothing
          }
      ),
      ( "s5e11",
        Dataset
          { dsName = "s5e11",
            dsType = Local,
            dsPath = "other/s5e11/test.csv",
            dsUrl = Nothing,
            dsKaggleDataset = Nothing,
            dsPMLBSetType = Nothing
          }
      ),
      ( "wine_quality",
        Dataset
          { dsName = "wine_quality",
            dsType = UCI,
            dsPath = "/tmp/wine_quality.csv",
            dsUrl = Just "https://mlr.cs.umass.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
            dsKaggleDataset = Nothing,
            dsPMLBSetType = Nothing
          }
      ),
      ( "iris",
        Dataset
          { dsName = "iris",
            dsType = UCI,
            dsPath = "/tmp/iris.csv",
            dsUrl = Just "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
            dsKaggleDataset = Nothing,
            dsPMLBSetType = Nothing
          }
      )
    ]

-- | Get a dataset by name
getDataset :: Text -> Maybe Dataset
getDataset name = Map.lookup name defaultDatasets

-- | Load dataset from disk or remote source
loadDataset :: Dataset -> IO D.DataFrame
loadDataset ds = case dsType ds of
  Local -> loadLocal ds
  UCI -> loadRemote ds
  Kaggle -> loadKaggle ds
  PMLB -> loadPMLB ds

-- | Load local CSV file
loadLocal :: Dataset -> IO D.DataFrame
loadLocal ds = D.readCsv (dsPath ds)

-- | Load from UCI ML repository (not yet implemented)
loadRemote :: Dataset -> IO D.DataFrame
loadRemote ds = case dsUrl ds of
  Nothing -> error $ "No URL specified for dataset: " <> T.unpack (dsName ds)
  Just url -> do
    putStrLn $ "Downloading " <> T.unpack (dsName ds) <> " from " <> url
    error "Remote dataset loading not yet implemented. Please download manually."

-- | Load from Kaggle (requires manual download for now)
loadKaggle :: Dataset -> IO D.DataFrame
loadKaggle ds = do
  putStrLn $ "Kaggle loading not yet implemented for dataset: " <> T.unpack (dsName ds)
  error $ "Please download manually and place in: " <> dsPath ds

-- | Load from PMLB (Penn ML Benchmarks)
loadPMLB :: Dataset -> IO D.DataFrame
loadPMLB _ = do
  error "PMLB loading not yet implemented"

-- | All classification dataset names
--
-- Source: https://github.com/EpistasisLab/pmlb/blob/master/pmlb/all_summary_stats.tsv
classificationNames :: [Text]
classificationNames =
  [ "GAMETES_Epistasis_2_Way_1000atts_0.4H_EDM_1_EDM_1_1",
    "GAMETES_Epistasis_2_Way_20atts_0.1H_EDM_1_1",
    "GAMETES_Epistasis_2_Way_20atts_0.4H_EDM_1_1",
    "GAMETES_Epistasis_3_Way_20atts_0.2H_EDM_1_1",
    "GAMETES_Heterogeneity_20atts_1600_Het_0.4_0.2_50_EDM_2_001",
    "GAMETES_Heterogeneity_20atts_1600_Het_0.4_0.2_75_EDM_2_001",
    "Hill_Valley_with_noise",
    "Hill_Valley_without_noise",
    "adult",
    "agaricus_lepiota",
    "allbp",
    "allhyper",
    "allhypo",
    "allrep",
    "analcatdata_aids",
    "analcatdata_asbestos",
    "analcatdata_authorship",
    "analcatdata_bankruptcy",
    "analcatdata_boxing1",
    "analcatdata_boxing2",
    "analcatdata_creditscore",
    "analcatdata_cyyoung8092",
    "analcatdata_cyyoung9302",
    "analcatdata_dmft",
    "analcatdata_fraud",
    "analcatdata_germangss",
    "analcatdata_happiness",
    "analcatdata_japansolvent",
    "analcatdata_lawsuit",
    "ann_thyroid",
    "appendicitis",
    "auto_insurance_symboling",
    "backache",
    "balance_scale",
    "banana",
    "biomed",
    "breast_cancer",
    "breast_cancer_wisconsin_diagnostic",
    "breast_cancer_wisconsin_original",
    "bupa",
    "calendarDOW",
    "car_evaluation",
    "cars",
    "chess",
    "churn",
    "clean1",
    "clean2",
    "cloud",
    "coil2000",
    "collins",
    "confidence",
    "congressional_voting_records",
    "connect_4",
    "contraceptive_method",
    "corral",
    "credit_approval_australia",
    "credit_approval_germany",
    "dermatology",
    "dis",
    "dna",
    "ecoli",
    "fars",
    "flags",
    "glass2",
    "haberman",
    "hayes_roth",
    "heart_disease_cleveland",
    "heart_disease_hungarian",
    "heart_disease_va_long_beach",
    "heart_disease_zurich",
    "hepatitis",
    "horse_colic_lesion_type",
    "horse_colic_outcome",
    "horse_colic_surgery",
    "hypothyroid",
    "ionosphere",
    "iris",
    "irish",
    "kddcup",
    "kr_vs_kp",
    "krkopt",
    "labor",
    "led24",
    "led7",
    "letter",
    "lupus",
    "lymphography",
    "magic",
    "mfeat_factors",
    "mfeat_fourier",
    "mfeat_karhunen",
    "mfeat_morphological",
    "mfeat_pixel",
    "mfeat_zernike",
    "mnist",
    "mofn_3_7_10",
    "molecular_biology_promoters",
    "monk1",
    "monk2",
    "monk3",
    "movement_libras",
    "mushroom",
    "mux6",
    "new_thyroid",
    "nursery",
    "optdigits",
    "page_blocks",
    "parity5",
    "parity5+5",
    "pendigits",
    "penguins",
    "phoneme",
    "poker",
    "postoperative_patient_data",
    "prnn_crabs",
    "prnn_synth",
    "profb",
    "ring",
    "saheart",
    "satimage",
    "schizo",
    "segmentation",
    "shuttle",
    "sleep",
    "sonar",
    "soybean",
    "spambase",
    "spect",
    "spectf",
    "splice",
    "tae",
    "texture",
    "threeOf9",
    "tic_tac_toe",
    "titanic",
    "tokyo1",
    "twonorm",
    "vehicle",
    "vowel",
    "waveform_21",
    "waveform_40",
    "wine_quality_red",
    "wine_quality_white",
    "wine_recognition",
    "xd6",
    "yeast"
  ]

-- | All regression dataset names
--
-- Source: https://github.com/EpistasisLab/pmlb/blob/master/pmlb/all_summary_stats.tsv
regressionNames :: [Text]
regressionNames =
  [ "1027_ESL",
    "1028_SWD",
    "1029_LEV",
    "1030_ERA",
    "1089_USCrime",
    "1096_FacultySalaries",
    "1191_BNG_pbc",
    "1193_BNG_lowbwt",
    "1196_BNG_pharynx",
    "1199_BNG_echoMonths",
    "1201_BNG_breastTumor",
    "1203_BNG_pwLinear",
    "1595_poker",
    "192_vineyard",
    "197_cpu_act",
    "201_pol",
    "210_cloud",
    "215_2dplanes",
    "218_house_8L",
    "225_puma8NH",
    "227_cpu_small",
    "228_elusage",
    "229_pwLinear",
    "230_machine_cpu",
    "294_satellite_image",
    "344_mv",
    "4544_GeographicalOriginalofMusic",
    "485_analcatdata_vehicle",
    "503_wind",
    "505_tecator",
    "519_vinnie",
    "522_pm10",
    "523_analcatdata_neavote",
    "527_analcatdata_election2000",
    "529_pollen",
    "537_houses",
    "542_pollution",
    "547_no2",
    "556_analcatdata_apnea2",
    "557_analcatdata_apnea1",
    "560_bodyfat",
    "561_cpu",
    "562_cpu_small",
    "564_fried",
    "573_cpu_act",
    "574_house_16H",
    "579_fri_c0_250_5",
    "581_fri_c3_500_25",
    "582_fri_c1_500_25",
    "583_fri_c1_1000_50",
    "584_fri_c4_500_25",
    "586_fri_c3_1000_25",
    "588_fri_c4_1000_100",
    "589_fri_c2_1000_25",
    "590_fri_c0_1000_50",
    "591_fri_c1_100_10",
    "592_fri_c4_1000_25",
    "593_fri_c1_1000_10",
    "594_fri_c2_100_5",
    "595_fri_c0_1000_10",
    "596_fri_c2_250_5",
    "597_fri_c2_500_5",
    "598_fri_c0_1000_25",
    "599_fri_c2_1000_5",
    "601_fri_c1_250_5",
    "602_fri_c3_250_10",
    "603_fri_c0_250_50",
    "604_fri_c4_500_10",
    "605_fri_c2_250_25",
    "606_fri_c2_1000_10",
    "607_fri_c4_1000_50",
    "608_fri_c3_1000_10",
    "609_fri_c0_1000_5",
    "611_fri_c3_100_5",
    "612_fri_c1_1000_5",
    "613_fri_c3_250_5",
    "615_fri_c4_250_10",
    "616_fri_c4_500_50",
    "617_fri_c3_500_5",
    "618_fri_c3_1000_50",
    "620_fri_c1_1000_25",
    "621_fri_c0_100_10",
    "622_fri_c2_1000_50",
    "623_fri_c4_1000_10",
    "624_fri_c0_100_5",
    "626_fri_c2_500_50",
    "627_fri_c2_500_10",
    "628_fri_c3_1000_5",
    "631_fri_c1_500_5",
    "633_fri_c0_500_25",
    "634_fri_c2_100_10",
    "635_fri_c0_250_10",
    "637_fri_c1_500_50",
    "641_fri_c1_500_10",
    "643_fri_c2_500_25",
    "644_fri_c4_250_25",
    "645_fri_c3_500_50",
    "646_fri_c3_500_10",
    "647_fri_c1_250_10",
    "648_fri_c1_250_50",
    "649_fri_c0_500_5",
    "650_fri_c0_500_50",
    "651_fri_c0_100_25",
    "653_fri_c0_250_25",
    "654_fri_c0_500_10",
    "656_fri_c1_100_5",
    "657_fri_c2_250_10",
    "658_fri_c3_250_25",
    "659_sleuth_ex1714",
    "663_rabe_266",
    "665_sleuth_case2002",
    "666_rmftsa_ladata",
    "678_visualizing_environmental",
    "687_sleuth_ex1605",
    "690_visualizing_galaxy",
    "695_chatfield_4",
    "706_sleuth_case1202",
    "712_chscase_geyser1",
    "auto_insurance_losses",
    "auto_insurance_price",
    "feynman_III_10_19",
    "feynman_III_12_43",
    "feynman_III_13_18",
    "feynman_III_14_14",
    "feynman_III_15_12",
    "feynman_III_15_14",
    "feynman_III_15_27",
    "feynman_III_17_37",
    "feynman_III_19_51",
    "feynman_III_21_20",
    "feynman_III_4_32",
    "feynman_III_4_33",
    "feynman_III_7_38",
    "feynman_III_8_54",
    "feynman_III_9_52",
    "feynman_II_10_9",
    "feynman_II_11_20",
    "feynman_II_11_27",
    "feynman_II_11_28",
    "feynman_II_11_3",
    "feynman_II_13_17",
    "feynman_II_13_23",
    "feynman_II_13_34",
    "feynman_II_15_4",
    "feynman_II_15_5",
    "feynman_II_21_32",
    "feynman_II_24_17",
    "feynman_II_27_16",
    "feynman_II_27_18",
    "feynman_II_2_42",
    "feynman_II_34_11",
    "feynman_II_34_2",
    "feynman_II_34_29a",
    "feynman_II_34_29b",
    "feynman_II_34_2a",
    "feynman_II_35_18",
    "feynman_II_35_21",
    "feynman_II_36_38",
    "feynman_II_37_1",
    "feynman_II_38_14",
    "feynman_II_38_3",
    "feynman_II_3_24",
    "feynman_II_4_23",
    "feynman_II_6_11",
    "feynman_II_6_15a",
    "feynman_II_6_15b",
    "feynman_II_8_31",
    "feynman_II_8_7",
    "feynman_I_10_7",
    "feynman_I_11_19",
    "feynman_I_12_1",
    "feynman_I_12_11",
    "feynman_I_12_2",
    "feynman_I_12_4",
    "feynman_I_12_5",
    "feynman_I_13_12",
    "feynman_I_13_4",
    "feynman_I_14_3",
    "feynman_I_14_4",
    "feynman_I_15_10",
    "feynman_I_15_3t",
    "feynman_I_15_3x",
    "feynman_I_16_6",
    "feynman_I_18_12",
    "feynman_I_18_14",
    "feynman_I_18_4",
    "feynman_I_24_6",
    "feynman_I_25_13",
    "feynman_I_26_2",
    "feynman_I_27_6",
    "feynman_I_29_16",
    "feynman_I_29_4",
    "feynman_I_30_3",
    "feynman_I_30_5",
    "feynman_I_32_17",
    "feynman_I_32_5",
    "feynman_I_34_1",
    "feynman_I_34_14",
    "feynman_I_34_27",
    "feynman_I_34_8",
    "feynman_I_37_4",
    "feynman_I_38_12",
    "feynman_I_39_1",
    "feynman_I_39_11",
    "feynman_I_39_22",
    "feynman_I_40_1",
    "feynman_I_41_16",
    "feynman_I_43_16",
    "feynman_I_43_31",
    "feynman_I_43_43",
    "feynman_I_44_4",
    "feynman_I_47_23",
    "feynman_I_48_2",
    "feynman_I_50_26",
    "feynman_I_6_2",
    "feynman_I_6_2a",
    "feynman_I_6_2b",
    "feynman_I_8_14",
    "feynman_I_9_18",
    "feynman_test_1",
    "feynman_test_10",
    "feynman_test_11",
    "feynman_test_12",
    "feynman_test_13",
    "feynman_test_14",
    "feynman_test_15",
    "feynman_test_16",
    "feynman_test_17",
    "feynman_test_18",
    "feynman_test_19",
    "feynman_test_2",
    "feynman_test_20",
    "feynman_test_3",
    "feynman_test_4",
    "feynman_test_5",
    "feynman_test_6",
    "feynman_test_7",
    "feynman_test_8",
    "feynman_test_9",
    "first_principles_absorption",
    "first_principles_bode",
    "first_principles_hubble",
    "first_principles_ideal_gas",
    "first_principles_kepler",
    "first_principles_leavitt",
    "first_principles_newton",
    "first_principles_planck",
    "first_principles_rydberg",
    "first_principles_schechter",
    "first_principles_supernovae_zg",
    "first_principles_supernovae_zr",
    "first_principles_tully_fisher",
    "nikuradse_1",
    "nikuradse_2",
    "solar_flare",
    "strogatz_bacres1",
    "strogatz_bacres2",
    "strogatz_barmag1",
    "strogatz_barmag2",
    "strogatz_glider1",
    "strogatz_glider2",
    "strogatz_lv1",
    "strogatz_lv2",
    "strogatz_predprey1",
    "strogatz_predprey2",
    "strogatz_shearflow1",
    "strogatz_shearflow2",
    "strogatz_vdp1",
    "strogatz_vdp2"
  ]

-- | Simple dataset registry: (name, url)
-- Used for browser dropdown and dataset selection
datasets :: [(Text, Text)]
datasets =
  [ ("penguins", "file://penguins.csv"),
    ("iris", "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"),
    ("wine_red", "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"),
    ("wine_white", "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"),
    ("titanic", "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"),
    ("flights", "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/flights.csv"),
    ("breast_cancer", "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),
    ("seoul_bikes", "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv")
  ]

-- | Get all dataset names for HTML dropdown
datasetNames :: [Text]
datasetNames = fmap fst datasets

-- | Look up a dataset URL by name
lookupUrl :: Text -> Maybe Text
lookupUrl name = lookup name datasets

-- | Get local saved datasets from ~/.cache/dataframe-load/ (excluding canned datasets)
localDatasets :: IO [Text]
localDatasets = do
  homeDir <- getHomeDirectory
  let cacheDir = homeDir </> ".cache" </> "dataframe-load"
  exists <- doesDirectoryExist cacheDir
  if not exists
    then pure []
    else do
      files <- listDirectory cacheDir
      let csvFiles = filter (\f -> takeExtension f == ".csv" || takeExtension f == "csv") files
      let localNames = fmap (T.pack . dropExtension) csvFiles
      -- Filter out names that are in the canned datasets list
      let canedNames = fmap fst datasets
      pure $ filter (\name -> name `notElem` canedNames) localNames
  where
    takeExtension = reverse . takeWhile (/= '.') . reverse
    dropExtension = reverse . drop 1 . dropWhile (/= '.') . reverse
