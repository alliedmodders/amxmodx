-- phpMyAdmin SQL Dump
-- version 4.1.12
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: Jul 31, 2014 at 09:34 PM
-- Server version: 5.6.16
-- PHP Version: 5.5.11

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `sm`
--

-- --------------------------------------------------------

--
-- Table structure for table `sm_smconst`
--

CREATE TABLE IF NOT EXISTS `sm_smconst` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fileid` int(11) NOT NULL,
  `descrip` tinytext NOT NULL,
  `fulltext` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fileid` (`fileid`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 AUTO_INCREMENT=81 ;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfiles`
--

CREATE TABLE IF NOT EXISTS `sm_smfiles` (
  `id` int(11) NOT NULL,
  `name` varchar(32) NOT NULL,
  `filename` varchar(32) NOT NULL,
  `fcount` int(11) NOT NULL,
  `ccount` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfilescon`
--

CREATE TABLE IF NOT EXISTS `sm_smfilescon` (
  `id` int(11) NOT NULL,
  `cont` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfunctions`
--

CREATE TABLE IF NOT EXISTS `sm_smfunctions` (
  `id` int(11) NOT NULL,
  `func` varchar(64) NOT NULL,
  `fullfunc` text NOT NULL,
  `description` text NOT NULL,
  `treturn` tinytext NOT NULL,
  `onerror` tinytext NOT NULL,
  `funcinput` text NOT NULL,
  `exemple` text NOT NULL,
  `inc` int(11) NOT NULL,
  `incname` varchar(32) NOT NULL,
  `typeof` int(11) NOT NULL,
  `depreached` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`func`),
  KEY `inc` (`inc`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
